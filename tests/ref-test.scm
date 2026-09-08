;;; Cellar -- tests for (cellar ref), the arithmetic the two halves share.
;;;
;;; Both halves of Cellar do this arithmetic and neither owns it: the kernel
;;; rewrites the references inside cell sources when a row moves, and the shell
;;; keeps the active cell on the same cell across the same move without waiting
;;; to be told where it went.  Two implementations of one rule is two chances to
;;; be wrong, and a disagreement between them does not show up as a crash -- it
;;; shows up as a formula quietly pointing at the wrong cell after a drag.
;;;
;;; So every case here has a twin in the "references" section of test/Spec.hs,
;;; written the same way round with the same numbers.  Change one side and the
;;; other should be changed with it.  Nothing here needs a display or a kernel.

(use-modules (cellar ref) (srfi srfi-1))

(define failures 0)
(define (check label expected actual)
  (if (equal? expected actual)
      (format #t "  ok   ~a~%" label)
      (begin (set! failures (+ failures 1))
             (format #t "  FAIL ~a: expected ~s got ~s~%" label expected actual))))

(format #t "-- how a reference is spelled~%")
(check "a column is letters" "A" (column->name 0))
(check "and carries" "AA" (column->name 26))
(check "a reference is written the usual way" "D6" (ref->name (make-ref 5 3)))
(check "and read back" (make-ref 5 3) (name->ref "D6"))
(check "a wide one too" (make-ref 29 26) (name->ref "AA30"))
(check "nonsense is not a reference" #f (name->ref "zzz"))
(check "nor is a bare number" #f (name->ref "12"))
(check "nor a row of nought" #f (name->ref "A0"))
(check "a column name reads back" 26 (name->column "AA"))
(check "and lowercase is not a column" #f (name->column "aa"))

;; The same sweep the Haskell suite makes, over the same range.
(check "every reference round trips"
       '()
       (filter (lambda (r) (not (equal? (name->ref (ref->name r)) r)))
               (append-map (lambda (row)
                             (map (lambda (column) (make-ref row column))
                                  (iota 41)))
                           (iota 41))))

(format #t "-- where a reference lands when the sheet is rearranged~%")
(check "a moved row takes its cells with it"
       (make-ref 0 3) (ref-after-move (make-ref 2 3) 'row 2 0))
(check "and the ones it passes slide over"
       (make-ref 1 3) (ref-after-move (make-ref 0 3) 'row 2 0))
(check "a column moves the same way"
       (make-ref 3 0) (ref-after-move (make-ref 3 2) 'column 2 0))
(check "a row left alone by a column move stays"
       (make-ref 3 2) (ref-after-move (make-ref 3 2) 'row 0 1))
(check "an insert pushes what is below it down"
       (make-ref 3 1) (ref-after-insert (make-ref 2 1) 'row 2))
(check "and leaves what is above alone"
       (make-ref 1 1) (ref-after-insert (make-ref 1 1) 'row 2))
(check "a column insert pushes what is beside it over"
       (make-ref 1 2) (ref-after-insert (make-ref 1 1) 'column 1))
(check "the index that moved lands where it was sent" 3 (shift-index 1 1 3))
(check "an index the move never reaches stays where it is" 5 (shift-index 5 1 3))
(check "an insert at an index moves it" 2 (shift-index-for-insert 1 1))
(check "and one after it does not" 0 (shift-index-for-insert 0 1))

;; A move and the move back are one permutation and its inverse, so together
;; they are nothing at all.  Cheaper to check over a small grid than to reason
;; about the four cases twice, once in each language.
(check "a move and the move back leave every index where it started"
       '()
       (filter (lambda (case)
                 (let ((i (first case)) (from (second case)) (to (third case)))
                   (not (= i (shift-index (shift-index i from to) to from)))))
               (append-map
                (lambda (i)
                  (append-map (lambda (from)
                                (map (lambda (to) (list i from to)) (iota 6)))
                              (iota 6)))
                (iota 6))))

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED"
                        (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
