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

(format #t "-- saving one cell at a time~%")
(let ((path (fresh "percell.cellar")))
  (create-sheet-directory! path #f)
  (save-cell! path "B2" "(* 6 7)")
  (check "a cell written by itself is a file of its own" #t
         (file-exists? (in path "cells" "B2.scm")))
  (check "holding its source" "(* 6 7)"
         (assoc-ref (read-sheet-cells path) "B2"))
  (save-cell! path "B2" "99")
  (check "writing it again replaces it" "99"
         (assoc-ref (read-sheet-cells path) "B2"))
  ;; An empty cell is a cell with no file, so emptying one has to take the file
  ;; away rather than leave an empty one behind.
  (save-cell! path "B2" "")
  (check "emptying a cell removes its file" #f
         (file-exists? (in path "cells" "B2.scm")))
  (check "and it is gone from the sheet on disk" '() (read-sheet-cells path))
  (save-cell! path "C3" "   ")
  (check "a cell of nothing but spaces never gets a file" #f
         (file-exists? (in path "cells" "C3.scm")))
  (save-cell! path "D4" "1")
  (check "emptying a cell that has no file is not an error" #f
         (begin (save-cell! path "Z9" "") (file-exists? (in path "cells" "Z9.scm")))))

(format #t "-- reading the sheet back without disturbing it~%")
(let ((path (fresh "reading.cellar")))
  (create-sheet-directory! path #f)
  (save-cell! path "A1" "1")
  (save-cell! path "A10" "10")
  (save-cell! path "A2" "2")
  ;; Sorted, so that two readings of an unchanged folder compare equal -- which
  ;; is what lets the watcher tell Cellar's own writes from everyone else's.
  (check "the cells come back sorted by name"
         '("A1" "A10" "A2") (map car (read-sheet-cells path)))
  (check "and reading twice gives the same thing"
         (read-sheet-cells path) (read-sheet-cells path))
  (write-sheet-metadata! path 120 30 '((1 . 181)))
  (let ((metadata (read-sheet-metadata path)))
    (check "the size is written" 120 (assq-ref metadata 'rows))
    (check "and the columns" 30 (assq-ref metadata 'columns))
    (check "and the widths" '((1 . 181)) (assq-ref metadata 'widths)))
  (check "writing the primary file leaves the cells alone"
         '("A1" "A10" "A2") (map car (read-sheet-cells path))))

(format #t "-- files that are not changing are not touched~%")
(let ((path (fresh "quiet.cellar")))
  (create-sheet-directory! path #f)
  (save-cell! path "A1" "1")
  (save-cell! path "A2" "2")
  (let ((sheet (make-sheet 4 2)))
    (load-sheet! sheet path)
    ;; Save once so that the files already say what the sheet says, then again
    ;; and see that the second save touched nothing.
    (save-sheet! sheet path '())
    (let ((cell (stat:mtime (stat (in path "cells" "A1.scm"))))
          (primary (stat:mtime (stat (in path "sheet.scm")))))
      (sleep 1)
      (save-sheet! sheet path '())
      (check "a whole-sheet save leaves an unchanged cell's mtime alone"
             cell (stat:mtime (stat (in path "cells" "A1.scm"))))
      (check "and the primary file's, when the shape did not change"
             primary (stat:mtime (stat (in path "sheet.scm"))))
      ;; A file that is changing is still written, mtime and all.
      (save-cell! path "A1" "changed")
      (check "a cell whose source did change is written"
             "changed" (assoc-ref (read-sheet-cells path) "A1"))
      (check "and its mtime moves" #t
             (> (stat:mtime (stat (in path "cells" "A1.scm"))) cell)))))

(format #t "-- making a directory and everything above it~%")
(let ((deep (in (fresh "deep") "a" "b" "c")))
  (make-directories! deep)
  (check "the whole chain is made" #t (file-exists? deep))
  (make-directories! deep)
  (check "and making it again is not an error" #t (file-exists? deep)))

(format #t "-- where a cell's file is~%")
(let ((path (fresh "paths.cellar")))
  (check "a cell's path is under cells/, named for the cell"
         (in path "cells" "B2.scm") (cell-file-path path "B2"))
  (check "and is answered for a cell that has no file yet" #t
         (string? (cell-file-path path "Z99"))))

(remove-tree root)

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED"
                        (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
