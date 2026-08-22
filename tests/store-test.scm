;;; Cellar -- tests for the on-disk format.
;;;
;;; A sheet is a directory of little files, so these tests write real ones into
;;; a temporary directory and read them back.  Nothing here needs a display.

(use-modules (cellar model) (cellar store) (srfi srfi-1)
             (ice-9 ftw) (ice-9 rdelim))

(define failures 0)
(define (check label expected actual)
  (if (equal? expected actual)
      (format #t "  ok   ~a~%" label)
      (begin (set! failures (+ failures 1))
             (format #t "  FAIL ~a: expected ~s got ~s~%" label expected actual))))

(define root (mkdtemp "/tmp/cellar-store-XXXXXX"))
(define (in root . parts) (string-join (cons root parts) "/"))
(define counter 0)
(define (fresh name)
  "A path inside the temporary directory that nothing has made yet."
  (set! counter (+ counter 1))
  (in root (format #f "~a-~a" counter name)))

(define (remove-tree path)
  (when (file-exists? path)
    (if (eq? 'directory (stat:type (lstat path)))
        (begin
          (for-each (lambda (entry)
                      (unless (member entry '("." ".."))
                        (remove-tree (string-append path "/" entry))))
                    (or (scandir path (lambda (n) #t)) '()))
          (rmdir path))
        (delete-file path))))

(define (contents path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((acc '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (string-join (reverse acc) "\n")
              (loop (cons line acc))))))))

(define (filled sheet cells)
  (for-each (lambda (entry)
              (set-cell-source! sheet (name->ref (car entry)) (cdr entry)))
            cells)
  sheet)


(format #t "-- making a sheet~%")
(let ((path (fresh "new.cellar")))
  (check "create reports what it did" 'created
         (create-sheet-directory! path #f))
  (check "there is a primary file" #t (file-exists? (in path "sheet.scm")))
  (check "and a place for the cells" #t (file-exists? (in path "cells")))
  (check "and it is a sheet" #t (sheet-directory? path))
  (check "creating over one is refused" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (create-sheet-directory! path #f) 'no-error)
           (lambda (key . args) key))))

(let ((path (fresh "git.cellar")))
  (let ((result (create-sheet-directory! path #t)))
    (check "create with git says so" #t
           (and (memq result '(created created-without-git)) #t))
    ;; git is normally there; when it is, the repository has to be real.
    (check "a repository when git ran" (eq? result 'created)
           (file-exists? (in path ".git")))))


(format #t "-- writing and reading~%")
(let ((path (fresh "round.cellar"))
      (sheet (filled (make-sheet 8 3)
                     '(("A1" . "\"Qty\"")
                       ("A2" . "7")
                       ("A3" . "5")
                       ("C1" . "(sum (range 'A2 'A3))")
                       ("C3" . "(if (> A2 5)\n    'over\n    'under)")))))
  (save-sheet! sheet path '((0 . 104) (2 . 180)))

  (check "a cell is a file of its source" "\"Qty\""
         (contents (in path "cells" "A1.scm")))
  (check "and so is a formula" "(sum (range 'A2 'A3))"
         (contents (in path "cells" "C1.scm")))
  (check "an empty cell has no file" #f (file-exists? (in path "cells" "B1.scm")))

  (let* ((copy (make-sheet 1 1))
         (widths (load-sheet! copy path)))
    (check "the sources come back" "\"Qty\"" (cell-source copy (name->ref "A1")))
    (check "several lines and all" "(if (> A2 5)\n    'over\n    'under)"
           (cell-source copy (name->ref "C3")))
    (check "the values with them" "12" (cell-display copy (name->ref "C1")))
    (check "the sheet is the size it was saved" 8 (sheet-rows copy))
    (check "in both directions" 3 (sheet-columns copy))
    (check "and the column widths are back" '((0 . 104) (2 . 180)) widths))

  (check "a sheet opens by its primary file too" #t
         (sheet-directory? (in path "sheet.scm")))
  (check "which names the same directory" path
         (sheet-directory (in path "sheet.scm"))))


(format #t "-- a save is a whole save~%")
(let ((path (fresh "stale.cellar"))
      (sheet (filled (make-sheet 8 3) '(("A1" . "1") ("A2" . "2") ("B1" . "3")))))
  (save-sheet! sheet path '())
  (check "three cells, three files" #t (file-exists? (in path "cells" "A2.scm")))

  (set-cell-source! sheet (name->ref "A2") "")
  (save-sheet! sheet path '())
  (check "clearing a cell takes its file away" #f
         (file-exists? (in path "cells" "A2.scm")))
  (check "leaving the others alone" #t (file-exists? (in path "cells" "A1.scm")))

  ;; A moved row renames files, which is the whole cost of naming cells by
  ;; position -- the point here is that nothing is left behind.
  (move-row! sheet 0 2)
  (save-sheet! sheet path '())
  (check "a move leaves no file behind" #f (file-exists? (in path "cells" "A1.scm")))
  (check "and writes the new name" "1" (contents (in path "cells" "A3.scm"))))


(format #t "-- what a save must not touch~%")
(let ((path (fresh "mine.cellar"))
      (sheet (filled (make-sheet 4 2) '(("A1" . "1")))))
  (save-sheet! sheet path '())
  (call-with-output-file (in path "README.md") (lambda (p) (display "mine\n" p)))
  (call-with-output-file (in path ".gitignore") (lambda (p) (display "*~\n" p)))
  (call-with-output-file (in path "cells" "notes.txt")
    (lambda (p) (display "not a cell\n" p)))
  (call-with-output-file (in path "cells" "helpers.scm")
    (lambda (p) (display "(define (double x) (* 2 x))\n" p)))

  (set-cell-source! sheet (name->ref "A1") "")
  (set-cell-source! sheet (name->ref "B2") "9")
  (save-sheet! sheet path '())

  (check "a README is left alone" #t (file-exists? (in path "README.md")))
  (check "so is a .gitignore" #t (file-exists? (in path ".gitignore")))
  (check "so is a file in cells/ that is not a cell" #t
         (file-exists? (in path "cells" "notes.txt")))
  (check "even one that looks like Scheme" #t
         (file-exists? (in path "cells" "helpers.scm")))
  (check "while the cleared cell did go" #f (file-exists? (in path "cells" "A1.scm")))
  (check "and the new one arrived" "9" (contents (in path "cells" "B2.scm"))))

(let ((path (fresh "odd.cellar"))
      (sheet (make-sheet 4 2)))
  (create-sheet-directory! path #f)
  (call-with-output-file (in path "cells" "a1.scm") (lambda (p) (display "lower\n" p)))
  (call-with-output-file (in path "cells" "A01.scm") (lambda (p) (display "padded\n" p)))
  (load-sheet! sheet path)
  (check "a file not spelled as the cell would be is not a cell" #f
         (cell-source sheet (name->ref "A1"))))


(format #t "-- opening what is not a sheet~%")
(let ((path (fresh "empty")))
  (mkdir path)
  (check "a bare directory is not a sheet" #f (sheet-directory? path))
  (check "and will not load" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (load-sheet! (make-sheet 4 2) path) 'no-error)
           (lambda (key . args) key))))

(remove-tree root)

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED"
                        (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
