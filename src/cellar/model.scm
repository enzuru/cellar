;;; Cellar -- the sheet model.
;;;
;;; A sheet maps cell references to Guile *source text*.  Values are produced by
;;; reading that text and evaluating it in a per-sheet sandbox module, so a cell
;;; is not a formula in a bespoke little language -- it is an expression.
;;;
;;; Sheets come in books.  A cell can name a cell on another sheet, which is
;;; what a book is for: without that, sheets that never mention each other might
;;; as well be in separate windows.  The book is also what makes a cross-sheet
;;; reference survive being rearranged -- see "Rearranging" below -- and what
;;; the cycle detector walks, since a cycle can now go round several sheets.
;;;
;;; This module deliberately knows nothing about GTK, so it can be exercised
;;; without a display.

(define-module (cellar model)
  #:use-module (cellar ref)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  ;; Reference arithmetic lives in (cellar ref), which the shell shares.  It is
  ;; re-exported here so that everything to do with a sheet still arrives from
  ;; one module.
  #:re-export (make-ref
               ref-row
               ref-column
               ref->name
               name->ref
               column->name
               ref-after-move
               ref-after-insert)
  #:export (make-book
            book?
            book-sheet
            book-sheet-names
            open-sheet!
            close-sheet!
            rename-sheet!
            make-sheet
            sheet?
            sheet-name
            sheet-rows
            sheet-columns
            cell-source
            set-cell-source!
            cell-value
            cell-style
            cell-display
            format-value
            preview-source
            cell-error?
            cell-error-message
            invalidate-sheet!
            move-row!
            move-column!
            insert-row!
            insert-column!
            grow-sheet!
            sheet-refs
            sheet->alist
            alist->sheet!
            valid-ref?
            reference-token
            written-reference))


;;;
;;; Errors
;;;

(define-record-type <cell-error>
  (make-cell-error message)
  cell-error?
  (message cell-error-message))


;;;
;;; Colour
;;;

;; A cell can dress its value: (styled (* B2 C2) #:background "#fff3b0").  The
;; style travels with the value rather than sitting in a table beside the
;; sheet, which is what makes conditional formatting an ordinary `if', and what
;; keeps colour through a save, a reload and a reordering -- there is nothing
;; to keep in step, because the colour is part of the expression.

(define-record-type <styled>
  (%make-styled value color background)
  styled?
  (value styled-value)
  (color styled-color)
  (background styled-background))

(define* (styled value #:key color background)
  "The sandbox's `styled'.  Wraps VALUE in the colours it asks to be drawn in.
Styling an already styled value merges the two, so (styled (styled x #:color
\"red\") #:background \"yellow\") is the obvious thing."
  (let ((color (check-color color))
        (background (check-color background)))
    (if (styled? value)
        (%make-styled (styled-value value)
                      (or color (styled-color value))
                      (or background (styled-background value)))
        (%make-styled value color background))))

(define (unstyle value)
  (if (styled? value) (styled-value value) value))

(define (check-color color)
  "COLOR, if it is one this program is willing to write into a stylesheet: a
hex literal like \"#c01c28\", or a colour name like \"red\".  Anything else
throws, so a typo shows up in the cell instead of quietly doing nothing -- and
nothing that could carry punctuation of its own reaches the CSS."
  (cond
   ((not color) #f)
   ((symbol? color) (check-color (symbol->string color)))
   ((and (string? color) (color-literal? color)) color)
   (else (throw 'cellar-error (format #f "styled: not a colour: ~s" color)))))

(define (color-literal? text)
  (let ((length (string-length text)))
    (if (and (> length 0) (char=? (string-ref text 0) #\#))
        (and (memv (- length 1) '(3 4 6 8))
             (string-every hex-digit? text 1))
        (and (> length 0) (string-every ascii-letter? text)))))

(define (hex-digit? c)
  (or (and (char>=? c #\0) (char<=? c #\9))
      (let ((c (char-downcase c)))
        (and (char>=? c #\a) (char<=? c #\f)))))

(define (ascii-letter? c)
  (let ((c (char-downcase c)))
    (and (char>=? c #\a) (char<=? c #\z))))


;;;
;;; The book
;;;

;; A book is the sheets that can see each other, by name.  The names are the
;; ones on the tabs: a cell says Summary!B2, so the kernel has to know which
;; sheet is called Summary, which is the one thing about tabs it does know.
;;
;; Two things live here rather than on a sheet because a sheet is no longer the
;; whole story.  The cache is dropped for every sheet at once, since a cell on
;; one sheet may be reading one on another and there is no dependency graph to
;; be cleverer with.  And the list of cells being evaluated is shared, because
;; a cycle can leave a sheet and come back to it.

(define-record-type <book>
  (%make-book sheets pending)
  book?
  (sheets book-sheets)
  (pending book-pending set-book-pending!))

(define (make-book) (%make-book (make-hash-table) '()))

;; The sheet record sits here, above the procedures that reach into it, because
;; a record accessor is a macro and a macro has to be in hand before it is used.
(define-record-type <sheet>
  (%make-sheet book name rows columns sources cache module)
  sheet?
  (book sheet-book)
  (name sheet-name set-sheet-name!)
  (rows sheet-rows set-sheet-rows!)
  (columns sheet-columns set-sheet-columns!)
  (sources sheet-sources)
  (cache sheet-cache)
  (module sheet-module set-sheet-module!))


(define (book-sheet book name)
  "The sheet BOOK calls NAME, or #f."
  (hash-ref (book-sheets book) name #f))

(define (book-sheet-list book)
  (hash-map->list (lambda (name s) s) (book-sheets book)))

(define (book-sheet-names book)
  "Every name in BOOK, in alphabetical order -- an order at all, rather than
whatever the hash table happens to say today."
  (sort (filter string? (hash-map->list (lambda (name s) name) (book-sheets book)))
        string<?))

(define (open-sheet! book name rows columns)
  "Put an empty sheet called NAME into BOOK and return it, replacing whatever
was called that.  Every other sheet is invalidated: any of them may name this
one, and what they had was about the sheet that has just been replaced."
  (let ((s (fresh-sheet book name rows columns)))
    (hash-set! (book-sheets book) name s)
    (invalidate-book! book)
    s))

(define (close-sheet! book name)
  "Take the sheet called NAME out of BOOK.  Cells elsewhere that name it become
errors, which is the truth and better than a stale number."
  (hash-remove! (book-sheets book) name)
  (invalidate-book! book))

(define (invalidate-book! book)
  "Drop every cached value in BOOK."
  (for-each (lambda (s) (hash-clear! (sheet-cache s))) (book-sheet-list book)))


;;;
;;; The sheet
;;;

(define (fresh-sheet book name rows columns)
  (let ((sheet (%make-sheet book name rows columns
                            (make-hash-table)
                            (make-hash-table)
                            #f)))
    (set-sheet-module! sheet (make-sandbox sheet))
    sheet))

(define (make-sheet rows columns)
  "A sheet on its own: a book of one, whose single sheet has no name.  Nothing
can name a nameless sheet, so this is a sheet that only sees itself -- which is
what a test of the model wants, and what the kernel never asks for."
  (open-sheet! (make-book) #f rows columns))

(define (valid-ref? sheet r)
  (and (pair? r)
       (>= (ref-row r) 0) (< (ref-row r) (sheet-rows sheet))
       (>= (ref-column r) 0) (< (ref-column r) (sheet-columns sheet))))

(define (cell-source sheet r)
  "The Guile source text of cell R, or #f when the cell is empty."
  (hash-ref (sheet-sources sheet) r #f))

(define (set-cell-source! sheet r text)
  "Set cell R's source to TEXT.  An empty or whitespace-only TEXT clears it.
Every cached value is dropped, since any cell may depend on this one."
  (let ((trimmed (and text (string-trim-both text))))
    (if (or (not trimmed) (string-null? trimmed))
        (hash-remove! (sheet-sources sheet) r)
        (hash-set! (sheet-sources sheet) r trimmed)))
  (invalidate-sheet! sheet))

(define (preview-source sheet r text)
  "Evaluate TEXT as if it were cell R's source, without committing it.
Used by the editor's live result preview."
  (let ((original (cell-source sheet r)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (set-cell-source! sheet r text)
        (cell-value sheet r))
      (lambda ()
        (if original
            (hash-set! (sheet-sources sheet) r original)
            (hash-remove! (sheet-sources sheet) r))
        (invalidate-sheet! sheet)))))

(define (invalidate-sheet! sheet)
  "Drop every cached value in the book this sheet is in, so the next read
recomputes.  The whole book, not the sheet: a cell on another sheet may be
reading this one, and nothing here knows which."
  (invalidate-book! (sheet-book sheet)))

(define (sheet-refs sheet)
  "All references that have source text, in row-major order."
  (sort (hash-map->list (lambda (k v) k) (sheet-sources sheet))
        (lambda (a b)
          (or (< (ref-row a) (ref-row b))
              (and (= (ref-row a) (ref-row b))
                   (< (ref-column a) (ref-column b)))))))


;;;
;;; How a reference is written
;;;

;; A reference is a token in someone's source text.  On its own sheet it is the
;; A1 it has always been; on another sheet it carries the sheet's name and a
;; bang: Summary!B2.  A sheet whose name has a space -- or a bracket, or a
;; quote, or anything else the reader would stop at -- cannot be spelled that
;; way, so it is written as the symbol it is: #{Q1 2026!B2}#.  That is Guile's
;; own syntax for such a symbol, which is the point of using it: the reader
;; takes it back whole, whatever is inside it, and a cell can go on saying
;;
;;     (+ Summary!B2 #{Q1 2026!B2}#)
;;
;; with both halves being ordinary variables.  The friendlier spelling for an
;; awkward name is (cell "Q1 2026" 'B2), which is understood too.

(define (reference-delimiter? c)
  (or (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\" #\' #\` #\, #\;))))

(define (extended-token? text)
  (and (>= (string-length text) 2) (string-prefix? "#{" text)))

(define (token-name text)
  "The characters TEXT spells as a symbol, or #f if it does not spell one."
  (if (not (extended-token? text))
      text
      (catch #t
        (lambda ()
          (call-with-input-string text
            (lambda (port)
              (let ((datum (read port)))
                (and (symbol? datum)
                     (eof-object? (read port))
                     (symbol->string datum))))))
        (lambda arguments #f))))

(define (written-reference text)
  "TEXT, a token out of a cell's source, as the reference it spells: a pair of
the sheet it names -- #f when it names none, and so means its own sheet -- and
the cell.  #f when the token is not a reference at all.

The split is at the *last* bang, so that a sheet called \"Wow!\" can be
referred to as Wow!!A1 and a symbol like set! is not mistaken for one."
  (let ((name (token-name text)))
    (and name
         (let ((bang (string-rindex name #\!)))
           (if (not bang)
               (let ((r (name->ref name))) (and r (cons #f r)))
               (let ((sheet (substring name 0 bang))
                     (r (name->ref (substring name (+ bang 1) (string-length name)))))
                 (and r (not (string-null? sheet)) (cons sheet r))))))))

(define (plain-token? text)
  "Can TEXT stand in source as itself: one token, read back as the symbol it
looks like?  Both halves matter -- the reader has to accept it, and the
rewriting below has to see it as a single token."
  (and (not (string-any reference-delimiter? text))
       (catch #t
         (lambda ()
           (call-with-input-string text
             (lambda (port)
               (let ((datum (read port)))
                 (and (symbol? datum)
                      (eof-object? (read port))
                      (string=? (symbol->string datum) text))))))
         (lambda arguments #f))))

(define (reference-token sheet r)
  "How a reference to SHEET's cell R is written.  SHEET is #f for a reference
on its own sheet."
  (if (not sheet)
      (ref->name r)
      (let ((text (string-append sheet "!" (ref->name r))))
        (if (plain-token? text) text (extended-token text)))))

(define (extended-token text)
  "TEXT in Guile's #{...}# symbol syntax, which is one token whatever is in it.

`write' would do this, and nearly does: it leaves a comma, a quote or a
backquote in a symbol unwrapped, because the reader is happy to take those back
as part of a symbol.  The rewriting above is not -- it splits a token at any of
them -- so the wrapping here is by the rule that matters to this file rather
than by the reader's."
  (string-append
   "#{"
   (string-concatenate (map escape-in-extended (string->list text)))
   "}#"))

(define (escape-in-extended c)
  ;; A closing brace would end the symbol early and a backslash would begin an
  ;; escape of its own.  Everything else stands as it is, spaces included.
  (if (memv c '(#\} #\\))
      (format #f "\\x~x;" (char->integer c))
      (string c)))

(define (written-sheet written home)
  "Which sheet WRITTEN is on, given that it is written on the sheet HOME."
  (or (car written) home))


;;;
;;; Rearranging
;;;

;; Moving a row is not just moving its cells: every reference to that row has
;; to follow the cell it names, or a sheet would change meaning the moment it
;; was rearranged.  So a move relocates the sources *and* rewrites the
;; references through the same permutation -- in every sheet of the book, not
;; just the one that moved, because Summary!B2 is as much a reference to
;; Summary's second row as B2 is when it is written there.
;;
;; Renaming a sheet is the same problem said differently: the cells it names
;; have not moved, but what they are called has, and every Summary!B2 in the
;; book has to become Totals!B2 or it is a reference to a sheet that is no
;; longer there.

(define (move-row! sheet from to)
  "Move row FROM to index TO, both 0-based.  Returns #t when the sheet changed,
or #f when the move is a no-op or out of range."
  (move-line! sheet 'row (sheet-rows sheet) from to))

(define (move-column! sheet from to)
  "Move column FROM to index TO, both 0-based.  Returns #t when the sheet
changed, or #f when the move is a no-op or out of range."
  (move-line! sheet 'column (sheet-columns sheet) from to))

(define (ref-index axis r)
  (if (eq? axis 'row) (ref-row r) (ref-column r)))

(define (move-line! sheet axis limit from to)
  "The common core of move-row! and move-column!."
  (and (integer? from) (integer? to)
       (>= from 0) (< from limit)
       (>= to 0) (< to limit)
       (not (= from to))
       (let ((relocate (lambda (r) (ref-after-move r axis from to))))
         (relocate-cells! sheet relocate)
         (rewrite-book!
          sheet relocate
          ;; A range is a rectangle, and the rectangle matters more than the
          ;; corners that describe it: reordering the lines of a table is not
          ;; supposed to change its subtotal.  So when a move begins and ends
          ;; inside a range, the range keeps its extent and only its contents
          ;; are shuffled; otherwise each corner follows its own cell, which is
          ;; what drops a row moved out of a range and picks up one moved in.
          (lambda (a b)
            (let ((low (min (ref-index axis a) (ref-index axis b)))
                  (high (max (ref-index axis a) (ref-index axis b))))
              (and (>= from low) (<= from high)
                   (>= to low) (<= to high)))))
         (invalidate-sheet! sheet)
         #t)))

(define (relocate-cells! sheet relocate)
  "Move every cell of SHEET to where RELOCATE says it goes.  The whole new
table is built before the old one is touched, since the permutation maps cells
onto keys that are still in use."
  (let* ((sources (sheet-sources sheet))
         (moved (hash-map->list (lambda (r text) (cons (relocate r) text))
                                sources)))
    (hash-clear! sources)
    (for-each (lambda (entry) (hash-set! sources (car entry) (cdr entry)))
              moved)))

(define (rewrite-book! target relocate whole-range?)
  "Rewrite every reference to a cell of sheet TARGET, wherever in the book it
is written, through RELOCATE.  WHOLE-RANGE? is asked about the corners of each
literal range on TARGET, and answers whether the range keeps its extent."
  (let ((moved (sheet-name target)))
    (for-each
     (lambda (s)
       (let ((home (sheet-name s)))
         (rewrite-sources!
          s
          (lambda (text)
            (let ((relocate-written
                   (lambda (written)
                     (if (equal? (written-sheet written home) moved)
                         (cons (car written) (relocate (cdr written)))
                         written))))
              (rewrite-refs text relocate-written
                            (explicit-spans text home moved relocate
                                            whole-range?)))))))
     (book-sheet-list (sheet-book target)))))

(define (rewrite-sources! sheet rewrite)
  (let* ((sources (sheet-sources sheet))
         (rewritten (hash-map->list (lambda (r text) (cons r (rewrite text)))
                                    sources)))
    (for-each (lambda (entry) (hash-set! sources (car entry) (cdr entry)))
              rewritten)))

(define (rewrite-refs text relocate spans)
  "Rewrite every cell reference in TEXT through RELOCATE, leaving everything
else -- spacing, comments, the shape of the code -- exactly as it was.  SPANS
holds the regions the caller has already decided for itself, as
(start end . replacement).

A reference is any token between delimiters that spells one, which covers both
the bare `A1' the evaluator binds as a variable and the quoted `'A1' that
(cell 'A1) and (range 'A1 'B2) are handed, and both of those with a sheet in
front of them.  Working on the text rather than on read data means a moved cell
comes back formatted the way it was written; the cost is that an A1 sitting in
a quoted list or an ordinary string is rewritten too."
  (let ((len (string-length text)))
    (let loop ((i 0) (pieces '()))
      (cond
       ((= i len) (string-concatenate (reverse pieces)))
       ;; A region the caller has already decided: a corner of a range, or a
       ;; reference inside a call that says which sheet it is on.
       ((assv i spans)
        => (lambda (span)
             (loop (cadr span) (cons (cddr span) pieces))))
       ;; A string is somebody's text, not code, and the sheet named in
       ;; (cell "Q1 2026" 'B2) is a string: rewriting inside one would turn
       ;; that sheet's name into another sheet's name on the first insert.
       ((char=? (string-ref text i) #\")
        (let ((end (string-literal-end text i len)))
          (loop end (cons (substring text i end) pieces))))
       ((reference-delimiter? (string-ref text i))
        (loop (+ i 1) (cons (substring text i (+ i 1)) pieces)))
       (else
        (let ((end (token-end text i len)))
          (loop end (cons (rewrite-token (substring text i end) relocate)
                          pieces))))))))

(define (string-literal-end text i len)
  "Where the string literal opening at I ends, counting the closing quote."
  (let scan ((j (+ i 1)))
    (cond ((>= j len) len)
          ((char=? (string-ref text j) #\\) (scan (+ j 2)))
          ((char=? (string-ref text j) #\") (+ j 1))
          (else (scan (+ j 1))))))

(define (token-end text i len)
  "Where the token starting at I ends.  A #{...}# symbol runs to its closing
brace whatever is inside it, since that is the whole point of the syntax; every
other token ends at the first delimiter."
  (if (and (< (+ i 1) len)
           (char=? (string-ref text i) #\#)
           (char=? (string-ref text (+ i 1)) #\{))
      (let scan ((j (+ i 2)))
        (cond ((>= (+ j 1) len) len)
              ((and (char=? (string-ref text j) #\})
                    (char=? (string-ref text (+ j 1)) #\#))
               (+ j 2))
              (else (scan (+ j 1)))))
      (let scan ((j i))
        (if (or (= j len) (reference-delimiter? (string-ref text j)))
            j
            (scan (+ j 1))))))

(define (rewrite-token token relocate)
  (let ((written (written-reference token)))
    (if written
        (let ((moved (relocate written)))
          (reference-token (car moved) (cdr moved)))
        token)))


;;;
;;; The references a call spells out for itself
;;;

;; Two calls name a sheet in a string rather than in front of the reference:
;; (cell "Q1 2026" 'B2) and (range "Q1 2026" 'A1 'B9).  The bare 'B2 in those
;; is not a reference to this sheet, so the token pass above must not treat it
;; as one, and the region it covers is handed over already decided -- unchanged
;; when the call names some other sheet, relocated when it names the one that
;; moved.  Literal ranges are here for the older reason: a range is a rectangle
;; and its corners are not independent.
;;
;; The cost of doing this on the text is that a reference has to be written
;; out to be found.  (cell some-name 'B2) is a cross-sheet reference this cannot
;; see, and will not follow a move.

(define %token-pattern "(#[{][^}]*[}]#|[^][()'\"`,; \t\n\r]+)")

(define %named-cell-call
  (make-regexp (string-append "\\(cell[[:space:]]+\"([^\"]*)\"[[:space:]]+'"
                              %token-pattern)))

(define %named-range-call
  (make-regexp (string-append "\\(range[[:space:]]+\"([^\"]*)\"[[:space:]]+'"
                              %token-pattern "[[:space:]]+'" %token-pattern)))

(define %range-call
  (make-regexp (string-append "\\(range[[:space:]]+'" %token-pattern
                              "[[:space:]]+'" %token-pattern)))

(define (matches regexp text)
  "Every match of REGEXP in TEXT."
  (let loop ((start 0) (found '()))
    (let ((m (and (<= start (string-length text))
                  (regexp-exec regexp text start))))
      (if (not m)
          (reverse found)
          (loop (max (match:end m) (+ start 1)) (cons m found))))))

(define (span m group replacement)
  (cons* (match:start m group) (match:end m group) replacement))

(define (explicit-spans text home moved relocate whole-range?)
  "The regions of TEXT the token pass must not decide for itself, each as
(start end . replacement).  HOME is the sheet TEXT lives on and MOVED the sheet
whose cells have moved."
  (append
   (append-map
    (lambda (m)
      (let ((named (match:substring m 1))
            (written (written-reference (match:substring m 2))))
        (if (or (not written) (car written))
            '()
            (list (span m 2 (if (equal? named moved)
                                (ref->name (relocate (cdr written)))
                                (match:substring m 2)))))))
    (matches %named-cell-call text))
   (append-map
    (lambda (m)
      (range-spans m 2 3 (match:substring m 1) moved relocate whole-range?))
    (matches %named-range-call text))
   (append-map
    (lambda (m)
      (let ((a (written-reference (match:substring m 1)))
            (b (written-reference (match:substring m 2))))
        (if (and a b (equal? (written-sheet a home) (written-sheet b home)))
            (range-spans m 1 2 (written-sheet a home) moved relocate whole-range?)
            '())))
    (matches %range-call text))))

(define (range-spans m first second named moved relocate whole-range?)
  "The two corners of one literal range, as spans."
  (let ((a (written-reference (match:substring m first)))
        (b (written-reference (match:substring m second))))
    (if (not (and a b))
        '()
        (let* ((keep (or (not (equal? named moved))
                         (whole-range? (cdr a) (cdr b))))
               (corner (lambda (written)
                         (if keep
                             (reference-token (car written) (cdr written))
                             (reference-token (car written)
                                              (relocate (cdr written)))))))
          (list (span m first (corner a))
                (span m second (corner b)))))))


;;;
;;; Inserting
;;;

;; Opening a row in the middle of a sheet is the same problem as moving one:
;; everything below it shifts, and every reference to what shifted has to shift
;; with it or the sheet changes meaning.  Ranges need no special case here.  A
;; range whose corners straddle the new line grows to take it in -- its far
;; corner moves down and its near one does not -- while one written entirely
;; below the new line simply follows it down, and both are what a spreadsheet
;; is expected to do.

(define (insert-row! sheet at)
  "Open an empty row at index AT, 0-based, pushing the rows from AT downwards
and growing the sheet by one.  AT may be the row count, which appends a row.
Returns #t when the sheet changed, #f when AT is out of range."
  (insert-line! sheet 'row at))

(define (insert-column! sheet at)
  "Open an empty column at index AT, 0-based, pushing the columns from AT
rightwards and growing the sheet by one.  AT may be the column count, which
appends a column.  Returns #t when the sheet changed, #f when AT is out of
range."
  (insert-line! sheet 'column at))

(define (insert-line! sheet axis at)
  "The common core of insert-row! and insert-column!."
  (let ((limit (if (eq? axis 'row) (sheet-rows sheet) (sheet-columns sheet))))
    (and (integer? at) (>= at 0) (<= at limit)
         (let ((relocate (lambda (r) (ref-after-insert r axis at))))
           (relocate-cells! sheet relocate)
           (rewrite-book! sheet relocate (lambda (a b) #f))
           (if (eq? axis 'row)
               (set-sheet-rows! sheet (+ limit 1))
               (set-sheet-columns! sheet (+ limit 1)))
           (invalidate-sheet! sheet)
           #t))))


;;;
;;; Renaming
;;;

(define (rename-sheet! book from to)
  "Call the sheet BOOK knows as FROM by the name TO, and say so everywhere it
is named.  Returns #t, or #f when there is no such sheet.

The cells do not move, so nothing is relocated; what changes is the name in
front of them, in every sheet including the one being renamed."
  (let ((s (book-sheet book from)))
    (and s
         (begin
           (for-each
            (lambda (other)
              (rewrite-sources!
               other
               (lambda (text)
                 (rewrite-refs
                  text
                  (lambda (written)
                    (if (equal? (car written) from)
                        (cons to (cdr written))
                        written))
                  (renamed-spans text from to)))))
            (book-sheet-list book))
           (hash-remove! (book-sheets book) from)
           (set-sheet-name! s to)
           (hash-set! (book-sheets book) to s)
           (invalidate-book! book)
           #t))))

(define (renamed-spans text from to)
  "The sheet names written out in (cell \"...\" 'A1) and (range \"...\" 'A1
'B2), where they say FROM, as spans that say TO.

The span takes in the quotes on either side, because the pass above steps over
a string literal in one go and would never look inside it."
  (append-map
   (lambda (m)
     (if (equal? (match:substring m 1) from)
         (list (cons* (- (match:start m 1) 1)
                      (+ (match:end m 1) 1)
                      (with-output-to-string (lambda () (write to)))))
         '()))
   (append (matches %named-cell-call text)
           (matches %named-range-call text))))


;;;
;;; Evaluation
;;;

(define %unset (list 'unset))

;; What an optional argument holds when it was not given.
(define %missing (list 'missing))

(define (cell-value sheet r)
  "The value of cell R, without whatever style it carries.  Returns #f for an
empty cell, or a <cell-error>."
  (unstyle (cell-raw-value sheet r)))

(define (cell-style sheet r)
  "The (colour . background) cell R asks to be drawn in, or #f.  Either half
of the pair can be #f on its own."
  (let ((value (cell-raw-value sheet r)))
    (and (styled? value)
         (let ((color (styled-color value))
               (background (styled-background value)))
           (and (or color background) (cons color background))))))

(define (cell-raw-value sheet r)
  "The value of cell R as its expression left it, style and all.
Values are memoised until the book is invalidated."
  (if (not (valid-ref? sheet r))
      (make-cell-error (format #f "reference out of range"))
      (let ((cached (hash-ref (sheet-cache sheet) r %unset)))
        (if (not (eq? cached %unset))
            cached
            (let ((value (compute-cell sheet r)))
              (hash-set! (sheet-cache sheet) r value)
              value)))))

(define (evaluating? book sheet r)
  (let loop ((rest (book-pending book)))
    (cond ((null? rest) #f)
          ((and (eq? (caar rest) sheet) (equal? (cdar rest) r)) #t)
          (else (loop (cdr rest))))))

(define (cycle-message book sheet r)
  "The cells that are waiting on each other, oldest first.  The sheet is named
only when the cycle leaves one, since inside a single sheet it would be noise
on every frame."
  (let* ((cycle (reverse (cons (cons sheet r) (book-pending book))))
         (crosses (any (lambda (entry) (not (eq? (car entry) sheet))) cycle)))
    (format #f "circular reference: ~a"
            (string-join
             (map (lambda (entry)
                    (if (and crosses (sheet-name (car entry)))
                        (string-append (sheet-name (car entry)) "!"
                                       (ref->name (cdr entry)))
                        (ref->name (cdr entry))))
                  cycle)
             " -> "))))

(define (compute-cell sheet r)
  (let ((source (cell-source sheet r))
        (book (sheet-book sheet)))
    (cond
     ((not source) #f)
     ((evaluating? book sheet r) (make-cell-error (cycle-message book sheet r)))
     (else
      (let ((saved (book-pending book)))
        (set-book-pending! book (cons (cons sheet r) saved))
        (let ((result (evaluate sheet source)))
          (set-book-pending! book saved)
          result))))))

(define (read-all str)
  "Read every datum in STR.  Returns #f if STR is not readable Scheme."
  (catch #t
    (lambda ()
      (call-with-input-string str
        (lambda (port)
          (let loop ((acc '()))
            (let ((datum (read port)))
              (if (eof-object? datum)
                  (reverse acc)
                  (loop (cons datum acc))))))))
    (lambda args #f)))

(define (evaluate sheet source)
  (let ((datums (read-all source)))
    (if (not datums)
        (make-cell-error "syntax error: unbalanced or malformed expression")
        (catch #t
          (lambda ()
            (let ((expr (auto-bind-refs sheet datums)))
              (eval expr (sheet-module sheet))))
          (lambda (key . args)
            (make-cell-error (describe-exception key args)))))))

(define (describe-exception key args)
  (match (cons key args)
    (('cellar-error message) message)
    ((_ subr message margs . _)
     (if (string? message)
         (let ((text (catch #t
                       (lambda ()
                         (with-output-to-string
                           (lambda ()
                             (display-error #f (current-output-port)
                                            subr message margs '()))))
                       (lambda _ message))))
           (string-trim-both
            ;; display-error prefixes with the subr; keep just the message.
            (if (string-null? text) message text)))
         (format #f "~a" key)))
    (_ (format #f "~a" key))))

;; Symbols that look like references -- A1, and Summary!B2 for a cell on
;; another sheet -- are bound to the value of the cell they name, so a cell can
;; simply say (+ A1 Summary!B2).  Quoted data is unaffected, since a `let'
;; binding does not reach inside a quote.
(define (auto-bind-refs sheet datums)
  (let ((refs (collect-refs sheet datums)))
    `(let ,(map (lambda (name+written)
                  `(,(car name+written)
                    (,'%cellar-lookup ',(cadr name+written) ',(cddr name+written))))
                refs)
       ,@datums)))

(define (collect-refs sheet datums)
  "The symbols in DATUMS that name cells, each with the sheet and cell it names.

A symbol with a sheet in front of it is bound whether or not that sheet is
open, so that the cell says which sheet it wanted rather than `unbound
variable'.  One without is bound only when the cell is on this sheet, which
leaves ZZ999 on a small sheet meaning whatever the surrounding code says it
means."
  (let ((seen '()))
    (let walk ((form datums))
      (cond
       ((symbol? form)
        (let ((written (written-reference (symbol->string form))))
          (when (and written
                     (or (car written) (valid-ref? sheet (cdr written)))
                     (not (assq form seen)))
            (set! seen (cons (cons form written) seen)))))
       ;; Quoted data is data.  (cell "Q1 2026" 'B2) names B2 on another
       ;; sheet, and binding the B2 inside it as though it were this sheet's
       ;; would evaluate a cell nobody asked for -- this one, when the cell
       ;; doing the asking is B2.
       ((and (pair? form) (eq? (car form) 'quote)) *unspecified*)
       ((pair? form) (walk (car form)) (walk (cdr form)))
       ((vector? form) (for-each walk (vector->list form)))
       (else *unspecified*)))
    (reverse seen)))


;;;
;;; The sandbox
;;;

(define (make-sandbox sheet)
  "A module in which cell expressions are evaluated.  It has all of (guile)
plus the sheet-aware helpers below."
  (let ((module (make-fresh-user-module)))
    (define (sheet-named who name)
      (or (and (not name) sheet)
          (book-sheet (sheet-book sheet) name)
          (throw 'cellar-error
                 (format #f "~a: no sheet called ~s is open" who name))))

    (define (label name r)
      (if name (string-append name "!" (ref->name r)) (ref->name r)))

    (define (lookup name r)
      (let* ((target (sheet-named (label name r) name))
             (value (cell-value target r)))
        (if (cell-error? value)
            (let ((message (cell-error-message value)))
              ;; A circular reference already names the whole cycle; prefixing
              ;; each frame again would just stutter.
              (throw 'cellar-error
                     (if (string-contains message "circular reference")
                         message
                         (format #f "~a: ~a" (label name r) message))))
            value)))

    (define (look place) (lookup (car place) (cdr place)))

    (define (coerce name value)
      (cond ((number? value) value)
            ((not value) 0)
            (else (throw 'cellar-error
                         (format #f "~a: expected a number, got ~s" name value)))))

    ;; A reference is written as a quoted symbol: 'A1, not "A1".  Having one
    ;; spelling keeps a string in a cell nothing but text, and means a move can
    ;; be trusted to find every reference it has to rewrite.  A sheet name is
    ;; the other way round -- (cell "Q1 2026" 'B2) -- for the same reason: the
    ;; rewriting has to be able to tell the two apart on sight.
    (define (parse-reference who r)
      (let ((written (cond ((and (pair? r) (integer? (car r))) (cons #f r))
                           ((symbol? r) (written-reference (symbol->string r)))
                           (else #f))))
        (cond (written written)
              ((string? r)
               (throw 'cellar-error
                      (format #f "~a: write the reference as a symbol, '~a" who r)))
              (else
               (throw 'cellar-error (format #f "~a: bad reference ~s" who r))))))

    (define (on-sheet who written r)
      "WRITTEN, if the sheet it is on has room for it."
      (if (valid-ref? (sheet-named who (car written)) (cdr written))
          written
          (throw 'cellar-error (format #f "~a: bad reference ~s" who r))))

    (define (resolve who r)
      (on-sheet who (parse-reference who r) r))

    (define (resolve-on who name r)
      "A reference read as being on the sheet NAME names.  The room it has to
fit in is that sheet's, which is why this cannot go through `resolve' -- the
sheet asked about may be bigger than the one asking."
      (unless (string? name)
        (throw 'cellar-error
               (format #f "~a: write the sheet as a string, ~s" who name)))
      ;; The rewriting that follows a move reads this call out of the text, and
      ;; it finds the sheet's name by the quotes around it.  A name with a
      ;; quote or a backslash of its own is one it would not find, so it is
      ;; refused here rather than followed quietly into a wrong answer.  The
      ;; symbol spelling works for every name.
      (when (or (string-index name #\") (string-index name #\\))
        (throw 'cellar-error
               (format #f "~a: write ~s in front of the reference, not as a string"
                       who name)))
      (let ((written (parse-reference who r)))
        (when (car written)
          (throw 'cellar-error (format #f "~a: ~s is already on a sheet" who r)))
        (on-sheet who (cons name (cdr written)) r)))

    (define* (cell-ref a #:optional (b %missing))
      (look (if (eq? b %missing) (resolve "cell" a) (resolve-on "cell" a b))))

    (define (range-refs who a b)
      (let ((r0 (min (ref-row (cdr a)) (ref-row (cdr b))))
            (r1 (max (ref-row (cdr a)) (ref-row (cdr b))))
            (c0 (min (ref-column (cdr a)) (ref-column (cdr b))))
            (c1 (max (ref-column (cdr a)) (ref-column (cdr b)))))
        (unless (equal? (car a) (car b))
          (throw 'cellar-error
                 (format #f "~a: ~a and ~a are on different sheets" who
                         (label (car a) (cdr a)) (label (car b) (cdr b)))))
        (append-map (lambda (row)
                      (map (lambda (column) (cons (car a) (make-ref row column)))
                           (iota (+ 1 (- c1 c0)) c0)))
                    (iota (+ 1 (- r1 r0)) r0))))

    (define* (range a b #:optional (c %missing))
      (map look (if (eq? c %missing)
                    (range-refs "range" (resolve "range" a) (resolve "range" b))
                    (range-refs "range" (resolve-on "range" a b)
                                (resolve-on "range" a c)))))

    (define (flatten args)
      (append-map (lambda (a) (if (list? a) (flatten a) (list a))) args))

    (define (numbers who args)
      (map (lambda (v) (coerce who v))
           (filter (lambda (v) (not (eq? v #f))) (flatten args))))

    (define (sum . args) (apply + (numbers "sum" args)))
    (define (product . args) (apply * (numbers "product" args)))
    (define (count . args) (length (filter number? (flatten args))))
    (define (average . args)
      (let ((ns (numbers "average" args)))
        (if (null? ns)
            (throw 'cellar-error "average: no numbers")
            (/ (apply + ns) (length ns)))))
    (define (smallest . args)
      (let ((ns (numbers "min" args)))
        (if (null? ns) (throw 'cellar-error "min: no numbers") (apply min ns))))
    (define (largest . args)
      (let ((ns (numbers "max" args)))
        (if (null? ns) (throw 'cellar-error "max: no numbers") (apply max ns))))

    (for-each (lambda (binding)
                (module-define! module (car binding) (cdr binding)))
              `((%cellar-lookup . ,lookup)
                (styled . ,styled)
                (cell . ,cell-ref)
                (range . ,range)
                (sum . ,sum)
                (product . ,product)
                (count . ,count)
                (average . ,average)
                (avg . ,average)
                (cell-min . ,smallest)
                (cell-max . ,largest)
                (this-sheet . ,sheet)))
    module))


;;;
;;; Presentation
;;;

(define (cell-display sheet r)
  "The string shown in the grid for cell R."
  (let ((value (cell-value sheet r)))
    (cond
     ((cell-error? value) "#ERR")
     ((not value) "")
     (else (format-value value)))))

(define (format-value value)
  (cond
   ((eq? value *unspecified*) "")
   ((string? value) value)
   ((and (number? value) (real? value) (not (exact? value))) (real->display value))
   ((number? value) (number->string value))
   ((eq? value #t) "#t")
   ((eq? value #f) "")
   ((symbol? value) (symbol->string value))
   ((char? value) (string value))
   (else (with-output-to-string (lambda () (write value))))))

(define %display-digits 12)

(define (real->display x)
  "Render an inexact real the way a spreadsheet should.
number->string is exact about doubles -- (* 7 19.99) really is
139.92999999999998 -- but showing that in a grid is just noise, so round to
~a significant digits first, and drop a trailing .0 while we are here."
  (cond
   ((not (finite? x)) (number->string x))
   ((= x (round x)) (if (< (abs x) 1e15)
                        (number->string (inexact->exact (round x)))
                        (number->string x)))
   (else
    (let ((rounded (round-significant x %display-digits)))
      (if (and (= rounded (round rounded)) (< (abs rounded) 1e15))
          (number->string (inexact->exact (round rounded)))
          (number->string rounded))))))

(define (round-significant x digits)
  (if (zero? x)
      x
      (let* ((magnitude (inexact->exact (floor (/ (log (abs x)) (log 10)))))
             (scale (expt 10 (- digits 1 magnitude))))
        (exact->inexact (/ (round (* (inexact->exact x) scale)) scale)))))

;;;
;;; Persistence -- a sheet is stored as a readable alist of (name . source).
;;;

(define (sheet->alist sheet)
  (map (lambda (r) (cons (ref->name r) (cell-source sheet r)))
       (sheet-refs sheet)))

(define (alist->sheet! sheet alist)
  (hash-clear! (sheet-sources sheet))
  ;; Grow to whatever the file needs.  A sheet that has had rows or columns
  ;; inserted into it is bigger than a fresh one, and reading it back into a
  ;; sheet of the default size would quietly drop the far edge of it.
  (for-each (lambda (entry)
              (let ((r (name->ref (car entry))))
                (when (and r (string? (cdr entry)))
                  (grow-to-fit! sheet r)
                  (hash-set! (sheet-sources sheet) r (cdr entry)))))
            alist)
  (invalidate-sheet! sheet))

(define (grow-sheet! sheet rows columns)
  "Make the sheet at least ROWS by COLUMNS.  A sheet only ever grows: opening a
smaller one into it leaves the empty room at the edges, which costs nothing."
  (when (> rows (sheet-rows sheet)) (set-sheet-rows! sheet rows))
  (when (> columns (sheet-columns sheet)) (set-sheet-columns! sheet columns)))

(define (grow-to-fit! sheet r)
  "Make the sheet big enough to hold reference R."
  (when (>= (ref-row r) (sheet-rows sheet))
    (set-sheet-rows! sheet (+ (ref-row r) 1)))
  (when (>= (ref-column r) (sheet-columns sheet))
    (set-sheet-columns! sheet (+ (ref-column r) 1))))
