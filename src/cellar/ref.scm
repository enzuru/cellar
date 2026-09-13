;;; Cellar -- cell references, and where they land when the sheet is rearranged.
;;;
;;; A reference is a (row . column) pair, both 0-based.  Users see them as "A1",
;;; where the column is a letter and the row is 1-based.
;;;
;;; This is the one module both halves of the program share.  The kernel needs
;;; references because it evaluates cells that name each other; the shell needs
;;; them because a cell's file is named for the cell, because the grid draws
;;; column headers, and because the active cell has to follow the row it is
;;; sitting on when that row moves.  Nothing here evaluates anything, touches a
;;; sheet, or knows what a sheet is -- it is arithmetic on pairs of integers and
;;; the spelling of them, which is why it can sit under both sides at once.
;;;
;;; A reference to a cell on another sheet -- Summary!B2 -- is not here.  It is
;;; spelled in (cellar model), because it names a sheet, and the shell is the
;;; half that never reads what is inside a cell.

(define-module (cellar ref)
  #:export (make-ref
            ref-row
            ref-column
            ref->name
            name->ref
            column->name
            name->column
            shift-index
            ref-after-move
            shift-index-for-insert
            ref-after-insert
            shift-index-for-delete
            ref-after-delete
            shift-index-past-delete
            ref-past-delete))

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
;;; Where a reference lands when the sheet is rearranged
;;;

;; Both halves need these and neither owns them.  The kernel rewrites the
;; references *inside* cell sources when a row moves; the shell keeps the active
;; cell on the same cell across the same move, without waiting to be told where
;; it went.  Same arithmetic, two callers.

(define (shift-index i from to)
  "Where index I lands when the item at FROM is moved to TO and the indices in
between slide over by one."
  (cond ((= i from) to)
        ((and (< from i) (<= i to)) (- i 1))
        ((and (<= to i) (< i from)) (+ i 1))
        (else i)))

(define (ref-after-move r axis from to)
  "Where reference R lands when FROM is moved to TO along AXIS."
  (if (eq? axis 'row)
      (make-ref (shift-index (ref-row r) from to) (ref-column r))
      (make-ref (ref-row r) (shift-index (ref-column r) from to))))

(define (shift-index-for-insert i at)
  "Where index I lands when a new line is opened at index AT."
  (if (>= i at) (+ i 1) i))

(define (ref-after-insert r axis at)
  "Where reference R lands when a line is inserted at AT along AXIS."
  (if (eq? axis 'row)
      (make-ref (shift-index-for-insert (ref-row r) at) (ref-column r))
      (make-ref (ref-row r) (shift-index-for-insert (ref-column r) at))))

;; Taking a line away is the one rearrangement with nowhere to send some of
;; what points at it.  A reference to the line itself has lost the cell it
;; named, and there are two honest things to do about that, depending on who is
;; asking.
;;
;; A reference written on its own -- the A3 in (+ A3 1) -- is dead, and
;; shift-index-for-delete says so by answering #f.  Pretending it now means
;; whatever slid up into row 3 would change what the cell computes without
;; saying a word.
;;
;; A corner of a range is not: A1:A5 with row 3 taken out is a range of four
;; rows, and a range that lost a corner would be a range nobody can write.  So
;; shift-index-past-delete carries the line's own index onto whatever took its
;; place, which shrinks the range from the far end and leaves the near one
;; alone.

(define (shift-index-for-delete i at)
  "Where index I lands when the line at AT is taken away, or #f when I is that
line and so is nowhere."
  (cond ((= i at) #f)
        ((> i at) (- i 1))
        (else i)))

(define (ref-after-delete r axis at)
  "Where reference R lands when the line at AT along AXIS is taken away, or #f
when R was on it."
  (let ((moved (shift-index-for-delete
                (if (eq? axis 'row) (ref-row r) (ref-column r))
                at)))
    (and moved
         (if (eq? axis 'row)
             (make-ref moved (ref-column r))
             (make-ref (ref-row r) moved)))))

(define (shift-index-past-delete i at)
  "Where index I lands when the line at AT is taken away and I is to follow
whatever took its place rather than die with it."
  (if (> i at) (- i 1) i))

(define (ref-past-delete r axis at)
  "Where reference R lands when the line at AT along AXIS is taken away, with a
reference to the line itself following whatever took its place."
  (if (eq? axis 'row)
      (make-ref (shift-index-past-delete (ref-row r) at) (ref-column r))
      (make-ref (ref-row r) (shift-index-past-delete (ref-column r) at))))
