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


;;;
;;; Workbooks
;;;

(format #t "-- making a workbook~%")
(let ((path (fresh "book.cellar")))
  (check "create reports what it did" 'created
         (create-workbook! path "Summary" #f))
  (check "and it is a workbook" #t (workbook-directory? path))
  (check "there is an index" #t (file-exists? (in path "workbook.scm")))
  (check "and a place for the sheets" #t (file-exists? (in path "sheets")))
  (check "holding the one it was given" '("Summary")
         (workbook-sheet-names path))
  (check "which is the one showing" "Summary" (workbook-active-sheet path))
  (check "the sheet is a sheet" #t
         (sheet-directory? (workbook-sheet-directory path "Summary")))
  (check "creating over one is refused" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (create-workbook! path "Summary" #f) 'no-error)
           (lambda (key . args) key))))

(format #t "-- adding sheets~%")
(let ((path (fresh "adding.cellar")))
  (create-workbook! path "Summary" #f)
  (check "a sheet can be added" "Q1" (add-workbook-sheet! path "Q1"))
  (add-workbook-sheet! path "Q2")
  (check "and turns up in order" '("Summary" "Q1" "Q2")
         (workbook-sheet-names path))
  (check "the one just added is the one showing" "Q2"
         (workbook-active-sheet path))
  (check "a name already taken is refused" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (add-workbook-sheet! path "Q1") 'no-error)
           (lambda (key . args) key)))
  (check "and so is one that differs only in case" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (add-workbook-sheet! path "q1") 'no-error)
           (lambda (key . args) key)))
  (check "a name a folder cannot have is refused" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (add-workbook-sheet! path "a/b") 'no-error)
           (lambda (key . args) key))))

(format #t "-- what a sheet may be called~%")
(check "an ordinary name" #t (valid-sheet-name? "Q1 2024"))
(check "not an empty one" #f (valid-sheet-name? "   "))
(check "not a path" #f (valid-sheet-name? "a/b"))
(check "not the folder itself" #f (valid-sheet-name? "."))
(check "not the one above it" #f (valid-sheet-name? ".."))
(check "not a hidden one" #f (valid-sheet-name? ".git"))

(format #t "-- suggesting a name~%")
(let ((path (fresh "naming.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (check "a free name is left alone" "Notes" (unique-sheet-name path "Notes"))
  (check "a taken one gains a number" "Summary 2"
         (unique-sheet-name path "Summary"))
  (check "and one that ends in a number counts on from it" "Q2"
         (unique-sheet-name path "Q1")))

(format #t "-- renaming a sheet~%")
(let ((path (fresh "renaming.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (let ((sheet (make-sheet 10 5)))
    (set-cell-source! sheet (name->ref "A1") "\"kept\"")
    (save-sheet! sheet (workbook-sheet-directory path "Q1") '()))
  (check "renaming reports the new name" "First Quarter"
         (rename-workbook-sheet! path "Q1" "First Quarter"))
  (check "the order is kept" '("Summary" "First Quarter")
         (workbook-sheet-names path))
  (check "the cells came with it" "\"kept\""
         (assoc-ref (read-sheet-cells
                     (workbook-sheet-directory path "First Quarter"))
                    "A1"))
  (check "and the old folder is gone" #f
         (file-exists? (in path "sheets" "Q1")))
  (check "renaming onto a taken name is refused" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (rename-workbook-sheet! path "First Quarter" "Summary")
                   'no-error)
           (lambda (key . args) key))))

(format #t "-- removing a sheet~%")
(let ((path (fresh "removing.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (add-workbook-sheet! path "Q2")
  (let ((sheet (make-sheet 10 5)))
    (set-cell-source! sheet (name->ref "A1") "1")
    (save-sheet! sheet (workbook-sheet-directory path "Q1") '()))
  (check "removing answers with what is left" '("Summary" "Q2")
         (remove-workbook-sheet! path "Q1"))
  (check "the folder went with it" #f (file-exists? (in path "sheets" "Q1")))
  (check "and the index no longer names it" '("Summary" "Q2")
         (workbook-sheet-names path))
  (remove-workbook-sheet! path "Q2")
  (check "the last sheet cannot be removed" 'cellar-store-error
         (catch 'cellar-store-error
           (lambda () (remove-workbook-sheet! path "Summary") 'no-error)
           (lambda (key . args) key)))
  (check "so the workbook still has one" '("Summary")
         (workbook-sheet-names path)))

(format #t "-- a note left in a sheet keeps its folder standing~%")
(let ((path (fresh "keepsake.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (call-with-output-file (in path "sheets" "Q1" "README")
    (lambda (port) (display "mine\n" port)))
  (remove-workbook-sheet! path "Q1")
  (check "the note is still there" #t
         (file-exists? (in path "sheets" "Q1" "README")))
  (check "but the sheet is not" #f
         (file-exists? (in path "sheets" "Q1" "sheet.scm")))
  (check "and so it is no longer a tab" '("Summary")
         (workbook-sheet-names path)))

(format #t "-- the order and the active sheet are remembered~%")
(let ((path (fresh "order.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (add-workbook-sheet! path "Q2")
  (set-workbook-order! path '("Q2" "Summary" "Q1"))
  (check "the order is what was written" '("Q2" "Summary" "Q1")
         (workbook-sheet-names path))
  (set-workbook-active! path "Summary")
  (check "and so is the active sheet" "Summary" (workbook-active-sheet path))
  (check "reordering does not disturb it" "Summary"
         (begin (set-workbook-order! path '("Summary" "Q1" "Q2"))
                (workbook-active-sheet path))))

(format #t "-- the index is a hint, and the disk is the truth~%")
(let ((path (fresh "hint.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  ;; As if someone else's commit had brought a sheet in and taken one away.
  (create-sheet-directory! (in path "sheets" "Arrived") #f)
  (remove-tree (in path "sheets" "Q1"))
  (check "a sheet the index never heard of turns up" #t
         (and (member "Arrived" (workbook-sheet-names path)) #t))
  (check "one whose folder went is dropped" #f
         (and (member "Q1" (workbook-sheet-names path)) #f))
  (check "and the ones it knows keep their order" "Summary"
         (car (workbook-sheet-names path))))

(format #t "-- a workbook from before there were tabs~%")
(let* ((path (fresh "old.cellar"))
       ;; Its one sheet borrows the folder's name, and `fresh' numbers that.
       (name (let ((base (basename path)))
               (substring base 0 (- (string-length base)
                                    (string-length ".cellar"))))))
  (create-sheet-directory! path #f)
  (let ((sheet (make-sheet 12 4)))
    (set-cell-source! sheet (name->ref "A1") "\"first\"")
    (set-cell-source! sheet (name->ref "B2") "(* 6 7)")
    (save-sheet! sheet path (list (cons 0 120))))
  (check "it is a workbook" #t (workbook-directory? path))
  (check "of the older shape" #t (workbook-format-1? path))
  (check "with one sheet, named for the folder" (list name)
         (workbook-sheet-names path))
  (check "which is the one showing" name (workbook-active-sheet path))
  (check "and lives where it always did" path
         (workbook-sheet-directory path name))
  (check "nothing was written to say so" #f
         (file-exists? (in path "workbook.scm")))

  ;; A second sheet is what moves it, and not before.
  (add-workbook-sheet! path "Q1")
  (check "adding a sheet moves the old one under sheets/" #t
         (file-exists? (in path "sheets" name "sheet.scm")))
  (check "with its cells" "(* 6 7)"
         (assoc-ref (read-sheet-cells (in path "sheets" name)) "B2"))
  (check "and its column widths" '((0 . 120))
         (let ((sheet (make-sheet 1 1)))
           (load-sheet! sheet (in path "sheets" name))))
  (check "the top of the workbook is clear" #f
         (file-exists? (in path "sheet.scm")))
  (check "there is an index now" #t (file-exists? (in path "workbook.scm")))
  (check "naming both sheets" (list name "Q1") (workbook-sheet-names path))
  (check "and it is no longer of the older shape" #f
         (workbook-format-1? path)))

(format #t "-- finding the workbook from inside it~%")
(let ((path (fresh "finding.cellar")))
  (create-workbook! path "Summary" #f)
  (check "from the folder itself" path (workbook-directory path))
  (check "from the index" path (workbook-directory (in path "workbook.scm")))
  (check "from a sheet's primary file" path
         (workbook-directory (in path "sheets" "Summary" "sheet.scm")))
  (check "a bare directory is not a workbook" #f
         (workbook-directory? (fresh "nothing"))))

(format #t "-- what has to be watched~%")
(let ((path (fresh "watching.cellar")))
  (create-workbook! path "Summary" #f)
  (add-workbook-sheet! path "Q1")
  (let ((paths (workbook-watch-paths path)))
    (check "the index" #t (and (member (in path "workbook.scm") paths) #t))
    (check "the folder the sheets are in" #t
           (and (member (in path "sheets") paths) #t))
    (check "each sheet's cells" #t
           (and (member (in path "sheets" "Q1" "cells") paths) #t))
    (check "and each sheet's primary file" #t
           (and (member (in path "sheets" "Summary" "sheet.scm") paths) #t))))

(remove-tree root)

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED"
                        (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
