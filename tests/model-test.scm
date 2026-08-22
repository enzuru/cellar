(use-modules (cellar model) (srfi srfi-1))
(define s (make-sheet 100 26))
(define (put! ref-name text) (set-cell-source! s (name->ref ref-name) text))
(define (shown ref-name) (cell-display s (name->ref ref-name)))
(define failures 0)
(define (check label expected actual)
  (if (equal? expected actual)
      (format #t "  ok   ~a~%" label)
      (begin (set! failures (+ failures 1))
             (format #t "  FAIL ~a: expected ~s got ~s~%" label expected actual))))

(format #t "-- references~%")
(check "A1" '(0 . 0) (name->ref "A1"))
(check "Z100" '(99 . 25) (name->ref "Z100"))
(check "roundtrip" "C7" (ref->name (name->ref "C7")))
(check "col AA" "AA" (column->name 26))
(check "bad ref" #f (name->ref "1A"))

(format #t "-- literals and arithmetic~%")
(put! "A1" "10")
(put! "A2" "32")
(put! "A3" "(+ A1 A2)")
(check "literal" "10" (shown "A1"))
(check "sum of refs" "42" (shown "A3"))

(format #t "-- recalculation~%")
(put! "A1" "100")
(check "recalc" "132" (shown "A3"))

(format #t "-- full Guile is available~%")
(put! "B1" "(string-upcase \"hello\")")
(put! "B2" "(apply + (map (lambda (n) (* n n)) (iota 5)))")
(put! "B3" "(if (> A3 100) 'big 'small)")
(check "string proc" "HELLO" (shown "B1"))
(check "higher order" "30" (shown "B2"))
(check "symbol" "big" (shown "B3"))

(format #t "-- helpers~%")
(put! "C1" "1") (put! "C2" "2") (put! "C3" "3")
(put! "C4" "(sum (range 'C1 'C3))")
(put! "C5" "(average (range 'C1 'C3))")
(put! "C6" "(cell 'C4)")
(check "sum range" "6" (shown "C4"))
(check "average" "2" (shown "C5"))
(check "cell fn" "6" (shown "C6"))

(format #t "-- empty cells~%")
(put! "D1" "")
(check "empty shows blank" "" (shown "D1"))
(put! "D6" "(sum (range 'D1 'D5))")
(check "empty counts as 0" "0" (shown "D6"))
(put! "D2" "(sum (range 'D1 'D5))")
(check "range containing self is a cycle" "#ERR" (shown "D2"))

(format #t "-- errors~%")
(put! "E1" "(/ 1 0)")
(check "div by zero" "#ERR" (shown "E1"))
(check "is error" #t (cell-error? (cell-value s (name->ref "E1"))))
(put! "E2" "(+ 1")
(check "syntax error" "#ERR" (shown "E2"))
(put! "E3" "(undefined-proc 1)")
(check "unbound" "#ERR" (shown "E3"))

(format #t "-- circular references~%")
(put! "F1" "(+ F2 1)")
(put! "F2" "(+ F1 1)")
(check "cycle detected" "#ERR" (shown "F1"))
(format #t "       message: ~a~%" (cell-error-message (cell-value s (name->ref "F1"))))
(put! "G1" "(+ G1 1)")
(check "self reference" "#ERR" (shown "G1"))

(format #t "-- quoted data is not rebound~%")
(put! "H1" "'(A1 B1)")
(check "quote intact" "(A1 B1)" (shown "H1"))

(format #t "-- colour~%")
(let ((r (make-sheet 8 3)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (shown name) (cell-display r (name->ref name)))
  (define (style name) (cell-style r (name->ref name)))
  (put! "A1" "(styled 42 #:color \"#c01c28\" #:background \"#fff3b0\")")
  (put! "A2" "(+ A1 1)")
  (put! "A3" "(styled (sum (range 'A1 'A2)) #:background \"yellow\")")
  (put! "B1" "(styled (styled 7 #:color 'red) #:background \"yellow\")")
  (put! "B2" "(if (> A1 10) (styled A1 #:background \"#3584e4\") A1)")
  (put! "B3" "(styled 1 #:color \"red; } * { color: green\")")
  (put! "C1" "(styled 2 #:color \"#12345\")")
  (put! "C2" "7")

  (check "a styled cell shows its value" "42" (shown "A1"))
  (check "and carries its colours" '("#c01c28" . "#fff3b0") (style "A1"))
  (check "a reference to it is just the value" "43" (shown "A2"))
  (check "so are the helpers" "85" (shown "A3"))
  (check "half a style is a style" '(#f . "yellow") (style "A3"))
  (check "styling a styled value merges the two" '("red" . "yellow") (style "B1"))
  (check "a colour can be a symbol" "7" (shown "B1"))
  (check "conditional formatting is an ordinary if"
         '(#f . "#3584e4") (style "B2"))
  (check "an unstyled cell has no style" #f (style "C2"))

  (check "a colour that is not one is an error" "#ERR" (shown "B3"))
  (format #t "       message: ~a~%"
          (cell-error-message (cell-value r (name->ref "B3"))))
  (check "and so is a malformed hex literal" "#ERR" (shown "C1"))

  ;; The style is part of the expression, so everything that moves the
  ;; expression moves the style with it, for free.
  (let ((copy (make-sheet 8 3)))
    (alist->sheet! copy (sheet->alist r))
    (check "colour survives a save and a load"
           '("#c01c28" . "#fff3b0") (cell-style copy (name->ref "A1"))))
  (move-row! r 0 2)
  (check "colour follows a moved row" '("#c01c28" . "#fff3b0") (style "A3"))
  (check "and the value with it" "42" (shown "A3")))

(format #t "-- persistence~%")
(let ((saved (sheet->alist s))
      (s2 (make-sheet 100 26)))
  (alist->sheet! s2 saved)
  (check "roundtrip A3" "132" (cell-display s2 (name->ref "A3")))
  (check "roundtrip C4" "6" (cell-display s2 (name->ref "C4"))))

(format #t "-- reordering~%")
(let ((r (make-sheet 10 5)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (src name) (cell-source r (name->ref name)))
  (define (shown-in name) (cell-display r (name->ref name)))
  (put! "A1" "1")
  (put! "A2" "2")
  (put! "A3" "3")
  (put! "B1" "(* A1 10)")
  (put! "B3" "(sum (range 'A1 'A2))")
  (put! "C1" "(list 'A1 my-A1)")

  (check "move-row! reports the move" #t (move-row! r 2 0))
  (check "the moved row is at its new index" "3" (src "A1"))
  (check "the rows it passed slid down" "1" (src "A2"))
  (check "cells move with their row" "(* A2 10)" (src "B2"))
  (check "references follow the cells they name" "10" (shown-in "B2"))
  (check "range endpoints are rewritten too"
         "(sum (range 'A2 'A3))" (src "B1"))
  (check "a moved range still covers the same cells" "3" (shown-in "B1"))
  (check "quoted refs move too, tokens that merely contain one do not"
         "(list 'A2 my-A1)" (src "C2"))

  (check "move-column! reports the move" #t (move-column! r 1 2))
  (check "cells move with their column" "(* A2 10)" (src "C2"))
  (check "the column it displaced slid left" "(list 'A2 my-A1)" (src "B2"))
  (check "values survive the move" "3" (shown-in "C1"))

  (check "a move onto itself does nothing" #f (move-row! r 1 1))
  (check "a move off the top does nothing" #f (move-row! r 0 -1))
  (check "a move past the last row does nothing" #f (move-row! r 0 10))
  (check "a move past the last column does nothing" #f (move-column! r 0 5))
  (check "the sheet is untouched by a refused move" "3" (src "A1")))

;; A range is a rectangle described by two corners, and the rectangle is what
;; the user means: reordering the lines of a table must not change its subtotal.
(let ((r (make-sheet 8 2)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (src name) (cell-source r (name->ref name)))
  (define (shown-in name) (cell-display r (name->ref name)))
  (put! "A1" "1")
  (put! "A2" "2")
  (put! "A3" "3")
  (put! "B4" "(sum (range 'A1 'A3))")
  (check "the range sums three cells to begin with" "6" (shown-in "B4"))

  (move-row! r 2 0)
  (check "a move inside a range leaves its corners alone"
         "(sum (range 'A1 'A3))" (src "B4"))
  (check "so the subtotal survives the reordering" "6" (shown-in "B4"))
  (check "while the cells themselves did move" "3" (src "A1"))

  ;; Row 1 (the 1) out to the bottom: it leaves the range behind.
  (move-row! r 1 7)
  (check "a row moved out of a range drops out of it"
         "(sum (range 'A1 'A2))" (src "B3"))
  (check "and the subtotal follows" "5" (shown-in "B3"))
  (check "the row itself is still on the sheet" "1" (src "A8")))

;; A reference has one spelling, and a string is not it.
(let ((r (make-sheet 8 2)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (shown-in name) (cell-display r (name->ref name)))
  (define (message name) (cell-error-message (cell-value r (name->ref name))))
  (put! "A1" "1")
  (put! "A2" "2")
  (put! "B1" "(cell \"A1\")")
  (put! "B2" "(sum (range \"A1\" \"A2\"))")

  (check "a string is not a reference to cell" "#ERR" (shown-in "B1"))
  (check "and says how to write one"
         "cell: write the reference as a symbol, 'A1" (message "B1"))
  (check "nor to range" "#ERR" (shown-in "B2"))
  (check "which says the same"
         "range: write the reference as a symbol, 'A1" (message "B2")))

(format #t "-- inserting rows and columns~%")

;; A new row is not just a blank line: what is below it moves down, and the
;; references to what moved follow it, exactly as they do for a reordering.
(let ((r (make-sheet 4 3)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (src name) (cell-source r (name->ref name)))
  (define (shown-in name) (cell-display r (name->ref name)))
  (put! "A1" "1")
  (put! "A2" "2")
  (put! "A3" "3")
  (put! "C1" "(* A3 10)")

  (check "insert-row! reports the insert" #t (insert-row! r 1))
  (check "the sheet is a row taller" 5 (sheet-rows r))
  (check "the rows above the new one stay put" "1" (src "A1"))
  (check "the new row is empty" #f (src "A2"))
  (check "the rows below it moved down" "2" (src "A3"))
  (check "references follow the cells they name" "(* A4 10)" (src "C1"))
  (check "so the value is unchanged" "30" (shown-in "C1"))

  (check "insert-column! reports the insert" #t (insert-column! r 1))
  (check "the sheet is a column wider" 4 (sheet-columns r))
  (check "the columns to the left stay put" "1" (src "A1"))
  (check "the new column is empty" #f (src "B1"))
  (check "the columns to the right moved over" "(* A4 10)" (src "D1"))

  (check "a row can be added at the end" #t (insert-row! r 5))
  (check "which makes the sheet taller too" 6 (sheet-rows r))
  (check "a row past the end is refused" #f (insert-row! r 7))
  (check "a negative row is refused" #f (insert-row! r -1))
  (check "and a column past the end is refused" #f (insert-column! r 5))
  (check "the sheet is untouched by a refused insert" 6 (sheet-rows r)))

;; A range is a rectangle, and inserting inside one grows it: the cells the
;; user drew a box around are still in the box, and the new line is in it too.
(let ((r (make-sheet 6 2)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (define (src name) (cell-source r (name->ref name)))
  (define (shown-in name) (cell-display r (name->ref name)))
  (put! "A1" "1")
  (put! "A2" "2")
  (put! "A3" "3")
  (put! "B5" "(sum (range 'A1 'A3))")
  (check "the range sums three cells to begin with" "6" (shown-in "B5"))

  (insert-row! r 1)
  (check "a row opened inside a range extends it"
         "(sum (range 'A1 'A4))" (src "B6"))
  (check "and the empty row adds nothing to the total" "6" (shown-in "B6"))
  (put! "A2" "10")
  (check "until it is filled in" "16" (shown-in "B6"))

  (insert-row! r 0)
  (check "a row opened above a range pushes it down, whole"
         "(sum (range 'A2 'A5))" (src "B7"))
  (check "with its total intact" "16" (shown-in "B7")))

;; The sheet grows to fit what is read into it, so an inserted row survives a
;; save and a load.
(let ((r (make-sheet 4 2)))
  (define (put! name text) (set-cell-source! r (name->ref name) text))
  (put! "A4" "(+ 1 1)")
  (insert-row! r 0)
  (check "the cell moved down with the insert" "(+ 1 1)" (cell-source r (name->ref "A5")))
  (let ((copy (make-sheet 4 2)))
    (alist->sheet! copy (sheet->alist r))
    (check "and a sheet too small to hold it grows on load" 5 (sheet-rows copy))
    (check "keeping the cell where it was" "2" (cell-display copy (name->ref "A5")))))

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED" (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
