;;; Cellar -- the sheet model.
;;;
;;; A sheet maps cell references to Guile *source text*.  Values are produced by
;;; reading that text and evaluating it in a per-sheet sandbox module, so a cell
;;; is not a formula in a bespoke little language -- it is an expression.
;;;
;;; This module deliberately knows nothing about GTK, so it can be exercised
;;; without a display.

(define-module (cellar model)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (make-sheet
            sheet?
            sheet-rows
            sheet-columns
            cell-source
            set-cell-source!
            cell-value
            cell-display
            format-value
            preview-source
            cell-error?
            cell-error-message
            invalidate-sheet!
            sheet-refs
            sheet->alist
            alist->sheet!
            make-ref
            ref-row
            ref-column
            ref->name
            name->ref
            column->name
            valid-ref?))


;;;
;;; Cell references
;;;

;; A reference is a (row . column) pair, both 0-based.  Users see them as "A1",
;; where the column is a letter and the row is 1-based.

(define (make-ref row column) (cons row column))
(define (ref-row r) (car r))
(define (ref-column r) (cdr r))

(define (column->name column)
  "Convert a 0-based COLUMN index to spreadsheet letters: 0 -> A, 26 -> AA."
  (let loop ((n column) (acc '()))
    (let ((letter (integer->char (+ (char->integer #\A) (remainder n 26))))
          (rest (quotient n 26)))
      (if (zero? rest)
          (list->string (cons letter acc))
          (loop (- rest 1) (cons letter acc))))))

(define (name->column str)
  "Inverse of column->name.  Returns #f if STR is not all A-Z."
  (and (> (string-length str) 0)
       (let loop ((i 0) (acc 0))
         (if (= i (string-length str))
             (- acc 1)
             (let ((c (string-ref str i)))
               (and (char>=? c #\A) (char<=? c #\Z)
                    (loop (+ i 1)
                          (+ (* acc 26)
                             (+ 1 (- (char->integer c) (char->integer #\A)))))))))))

(define (ref->name r)
  (string-append (column->name (ref-column r))
                 (number->string (+ 1 (ref-row r)))))

(define (name->ref name)
  "Parse \"A1\" into a reference, or return #f."
  (let* ((str (if (symbol? name) (symbol->string name) name))
         (len (string-length str))
         (split (let loop ((i 0))
                  (cond ((= i len) #f)
                        ((char-numeric? (string-ref str i)) i)
                        (else (loop (+ i 1)))))))
    (and split
         (> split 0)
         (< split len)
         (let ((column (name->column (substring str 0 split)))
               (row (string->number (substring str split len))))
           (and column
                row
                (exact? row)
                (integer? row)
                (>= row 1)
                (make-ref (- row 1) column))))))


;;;
;;; Errors
;;;

(define-record-type <cell-error>
  (make-cell-error message)
  cell-error?
  (message cell-error-message))


;;;
;;; The sheet
;;;

(define-record-type <sheet>
  (%make-sheet rows columns sources cache pending module)
  sheet?
  (rows sheet-rows)
  (columns sheet-columns)
  (sources sheet-sources)
  (cache sheet-cache)
  ;; References currently being evaluated, used to detect circular references.
  (pending sheet-pending set-sheet-pending!)
  (module sheet-module set-sheet-module!))

(define (make-sheet rows columns)
  (let ((sheet (%make-sheet rows columns
                            (make-hash-table)
                            (make-hash-table)
                            '()
                            #f)))
    (set-sheet-module! sheet (make-sandbox sheet))
    sheet))

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
  "Drop every cached value, so the next read recomputes."
  (hash-clear! (sheet-cache sheet)))

(define (sheet-refs sheet)
  "All references that have source text, in row-major order."
  (sort (hash-map->list (lambda (k v) k) (sheet-sources sheet))
        (lambda (a b)
          (or (< (ref-row a) (ref-row b))
              (and (= (ref-row a) (ref-row b))
                   (< (ref-column a) (ref-column b)))))))


;;;
;;; Evaluation
;;;

(define %unset (list 'unset))

(define (cell-value sheet r)
  "The value of cell R.  Returns #f for an empty cell, or a <cell-error>.
Values are memoised until the sheet is invalidated."
  (if (not (valid-ref? sheet r))
      (make-cell-error (format #f "reference out of range"))
      (let ((cached (hash-ref (sheet-cache sheet) r %unset)))
        (if (not (eq? cached %unset))
            cached
            (let ((value (compute-cell sheet r)))
              (hash-set! (sheet-cache sheet) r value)
              value)))))

(define (compute-cell sheet r)
  (let ((source (cell-source sheet r)))
    (cond
     ((not source) #f)
     ((member r (sheet-pending sheet))
      (make-cell-error
       (format #f "circular reference: ~a"
               (string-join (map ref->name (reverse (cons r (sheet-pending sheet))))
                            " -> "))))
     (else
      (let ((saved (sheet-pending sheet)))
        (set-sheet-pending! sheet (cons r saved))
        (let ((result (evaluate sheet source)))
          (set-sheet-pending! sheet saved)
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

;; Symbols that look like references ("A1") are bound to the value of the cell
;; they name, so a cell can simply say (+ A1 B1).  Quoted data is unaffected,
;; since a `let' binding does not reach inside a quote.
(define (auto-bind-refs sheet datums)
  (let ((refs (collect-refs sheet datums)))
    `(let ,(map (lambda (name+ref)
                  `(,(car name+ref)
                    (,'%cellar-lookup ',(cdr name+ref))))
                refs)
       ,@datums)))

(define (collect-refs sheet datums)
  (let ((seen '()))
    (let walk ((form datums))
      (cond
       ((symbol? form)
        (let ((r (name->ref form)))
          (when (and r (valid-ref? sheet r) (not (assq form seen)))
            (set! seen (cons (cons form r) seen)))))
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
    (define (lookup r)
      (let ((value (cell-value sheet r)))
        (if (cell-error? value)
            (let ((message (cell-error-message value)))
              ;; A circular reference already names the whole cycle; prefixing
              ;; each frame again would just stutter.
              (throw 'cellar-error
                     (if (string-contains message "circular reference")
                         message
                         (format #f "~a: ~a" (ref->name r) message))))
            value)))

    (define (coerce name value)
      (cond ((number? value) value)
            ((not value) 0)
            (else (throw 'cellar-error
                         (format #f "~a: expected a number, got ~s" name value)))))

    (define (resolve who r)
      (let ((parsed (cond ((pair? r) r)
                          ((or (string? r) (symbol? r)) (name->ref r))
                          (else #f))))
        (or (and parsed (valid-ref? sheet parsed) parsed)
            (throw 'cellar-error (format #f "~a: bad reference ~s" who r)))))

    (define (cell-ref r) (lookup (resolve "cell" r)))

    (define (range-refs from to)
      (let* ((a (resolve "range" from))
             (b (resolve "range" to))
             (r0 (min (ref-row a) (ref-row b)))
             (r1 (max (ref-row a) (ref-row b)))
             (c0 (min (ref-column a) (ref-column b)))
             (c1 (max (ref-column a) (ref-column b))))
        (append-map (lambda (row)
                      (map (lambda (column) (make-ref row column))
                           (iota (+ 1 (- c1 c0)) c0)))
                    (iota (+ 1 (- r1 r0)) r0))))

    (define (range from to) (map lookup (range-refs from to)))

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
  (for-each (lambda (entry)
              (let ((r (name->ref (car entry))))
                (when (and r (valid-ref? sheet r) (string? (cdr entry)))
                  (hash-set! (sheet-sources sheet) r (cdr entry)))))
            alist)
  (invalidate-sheet! sheet))
