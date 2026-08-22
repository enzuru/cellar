;;; Cellar -- sheets on disk.
;;;
;;; A sheet is a directory, not a file.  Every cell that holds anything is one
;;; small file of Guile source under cells/, named for the cell, and a primary
;;; file at the top holds what is true of the sheet rather than of any one cell.
;;;
;;;     budget.cellar/
;;;       sheet.scm        the size of the sheet, and the column widths
;;;       cells/
;;;         A1.scm         Qty
;;;         D6.scm         (sum (range 'D2 'D4))
;;;
;;; The point of it is version control.  A cell already holds source text, so
;;; giving each one a file makes an edit to a cell a one-line diff, a cell's
;;; history `git log -p cells/D6.scm', and two people editing different corners
;;; of a sheet a merge rather than a conflict.
;;;
;;; The cost is that a cell's name is its position, so inserting a row renames
;;; every file below it and rewrites every reference to them.  That is a loud
;;; diff, but an honest one: the sheet really did change shape, and the
;;; references really did all change with it.
;;;
;;; Like (cellar model), this module knows nothing about GTK.

(define-module (cellar store)
  #:use-module (cellar model)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:export (sheet-directory
            sheet-directory?
            create-sheet-directory!
            save-sheet!
            load-sheet!
            sheet-name))

(define %primary-file "sheet.scm")
(define %cells-directory "cells")
(define %cell-suffix ".scm")
(define %format 1)

(define (sheet-directory path)
  "The sheet directory PATH names.  A sheet can be opened by its directory or
by the primary file inside it, and both arrive here as the directory."
  (if (and (file-exists? path)
           (not (directory? path))
           (string=? (basename path) %primary-file))
      (dirname path)
      path))

(define (sheet-directory? path)
  "Is PATH a Cellar sheet -- a directory with a primary file in it?"
  (let ((directory (sheet-directory path)))
    (and (file-exists? directory)
         (directory? directory)
         (file-exists? (primary-file directory)))))

(define (sheet-name path)
  "What to call the sheet at PATH in a window title."
  (basename (sheet-directory path)))

(define (directory? path)
  (eq? 'directory (stat:type (stat path))))

(define (primary-file directory)
  (string-append directory "/" %primary-file))

(define (cells-directory directory)
  (string-append directory "/" %cells-directory))

(define (cell-file directory name)
  (string-append (cells-directory directory) "/" name %cell-suffix))


;;;
;;; Making one
;;;

(define (create-sheet-directory! directory git?)
  "Make an empty sheet at DIRECTORY, and a git repository of it when GIT? is
true.  Returns 'created, or 'created-without-git when git could not be run --
a sheet without a repository is still a sheet, so that is not a failure.
Throws if the directory cannot be made or already holds a sheet."
  (when (sheet-directory? directory)
    (throw 'cellar-store-error
           (format #f "~a is already a Cellar sheet" directory)))
  (ensure-directory! directory)
  (ensure-directory! (cells-directory directory))
  (write-primary directory 0 0 '())
  (if (and git? (not (git-init directory)))
      'created-without-git
      'created))

(define (git-init directory)
  "Run `git init' in DIRECTORY.  Returns #f if git is not there to run or says
no; the sheet itself is already written by then, so the caller carries on."
  (catch #t
    (lambda ()
      (eqv? 0 (status:exit-val
               (system* "git" "init" "--quiet" directory))))
    (lambda args #f)))

(define (ensure-directory! path)
  (unless (file-exists? path)
    (mkdir path)))


;;;
;;; Writing
;;;

(define (save-sheet! sheet directory widths)
  "Write SHEET to DIRECTORY: the primary file, a file for every cell that holds
something, and no file for any cell that does not.  WIDTHS is an alist of
column index to pixel width."
  (ensure-directory! directory)
  (ensure-directory! (cells-directory directory))
  (write-primary directory (sheet-rows sheet) (sheet-columns sheet) widths)
  (let* ((cells (sheet->alist sheet))
         (live (map car cells)))
    (for-each (lambda (entry) (write-cell directory (car entry) (cdr entry)))
              cells)
    (for-each (lambda (name)
                (unless (member name live)
                  (delete-file (cell-file directory name))))
              (stored-cell-names directory))))

(define (write-primary directory rows columns widths)
  ;; An entry to a line, so that changing the size of a sheet is a one-line
  ;; diff rather than a rewritten file.
  (call-with-output-file (primary-file directory)
    (lambda (port)
      (display ";; A Cellar sheet. The cells are in cells/, one file each.\n"
               port)
      (let loop ((entries `((format . ,%format)
                            (rows . ,rows)
                            (columns . ,columns)
                            (widths . ,widths)))
                 (opening "("))
        (unless (null? entries)
          (display opening port)
          (write (car entries) port)
          (if (null? (cdr entries))
              (display ")\n" port)
              (begin (newline port) (loop (cdr entries) " "))))))))

(define (write-cell directory name source)
  (call-with-output-file (cell-file directory name)
    (lambda (port)
      (display source port)
      ;; A trailing newline: these are text files, and diffs of files without
      ;; one are a nuisance to read.
      (unless (string-suffix? "\n" source)
        (newline port)))))


;;;
;;; Reading
;;;

(define (load-sheet! sheet directory)
  "Read the sheet at DIRECTORY into SHEET, replacing what was there.  Returns
the column widths it was saved with, as an alist of column index to width."
  (unless (sheet-directory? directory)
    (throw 'cellar-store-error
           (format #f "~a is not a Cellar sheet" directory)))
  (let ((metadata (read-primary directory)))
    (alist->sheet! sheet
                   (map (lambda (name)
                          (cons name (read-cell directory name)))
                        (stored-cell-names directory)))
    ;; The sheet is at least as big as it was saved: a sheet can be taller than
    ;; its last full row, and those empty rows are part of what was saved.
    (grow-sheet! sheet
                 (or (assq-ref metadata 'rows) 0)
                 (or (assq-ref metadata 'columns) 0))
    (or (assq-ref metadata 'widths) '())))

(define (read-primary directory)
  (let ((datum (catch #t
                 (lambda () (call-with-input-file (primary-file directory) read))
                 (lambda args #f))))
    (if (list? datum) datum '())))

(define (read-cell directory name)
  "The source in a cell's file.  Trimmed, both because the file is written with
a newline this has to take back off and because a cell typed into the app is
trimmed too -- a file edited by hand should behave the same as one that was
not."
  (call-with-input-file (cell-file directory name)
    (lambda (port)
      (let ((text (get-string-all port)))
        (if (eof-object? text) "" (string-trim-both text))))))

(define (stored-cell-names directory)
  "The cells with a file in DIRECTORY, by name.

Only files named for a cell are answered with, and so only those are ever
deleted by a save.  A sheet directory is a place a person can keep a README or
a .gitignore, and nothing here may touch them."
  (let ((cells (cells-directory directory)))
    (if (not (file-exists? cells))
        '()
        (filter-map cell-file-name
                    (or (scandir cells (lambda (name) #t)) '())))))

(define (cell-file-name file)
  "The cell FILE is named for, or #f when it is named for no cell at all."
  (and (string-suffix? %cell-suffix file)
       (let ((name (substring file 0
                              (- (string-length file)
                                 (string-length %cell-suffix)))))
         (and (name->ref name)
              ;; name->ref is lenient about what it will parse; the file has to
              ;; be spelled exactly the way the cell would be written.
              (string=? name (ref->name (name->ref name)))
              name))))
