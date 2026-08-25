;;; Cellar -- sheets on disk.
;;;
;;; A sheet is a directory, not a file.  Every cell that holds anything is one
;;; small file of Guile source under cells/, named for the cell, and a primary
;;; file at the top holds what is true of the sheet rather than of any one cell.
;;;
;;; A workbook is a directory of those -- what a tab bar shows, and what a git
;;; repository holds one of:
;;;
;;;     budget.cellar/
;;;       workbook.scm     which sheets there are, and in what order
;;;       sheets/
;;;         Summary/
;;;           sheet.scm    the size of the sheet, and the column widths
;;;           cells/
;;;             A1.scm     Qty
;;;             D6.scm     (sum (range 'D2 'D4))
;;;         Q1/
;;;           sheet.scm
;;;           cells/
;;;
;;; The layer above is deliberately thin: a sheet folder is exactly what it
;;; always was, and everything below that reads or writes one neither knows nor
;;; cares whether it sits at the top of a workbook or inside sheets/.  That is
;;; what lets a workbook written before tabs existed -- a bare sheet.scm and
;;; cells/ at the top -- go on being read where it lies, and be moved into
;;; sheets/ only when a second sheet gives it a reason to.
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
            save-cell!
            cell-file-path
            write-sheet-metadata!
            read-sheet-cells
            read-sheet-metadata
            load-sheet!
            make-directories!
            sheet-name

            workbook-directory
            workbook-directory?
            workbook-name
            workbook-format-1?
            create-workbook!
            read-workbook
            workbook-sheet-names
            workbook-active-sheet
            workbook-sheet-directory
            write-workbook-index!
            set-workbook-active!
            set-workbook-order!
            add-workbook-sheet!
            rename-workbook-sheet!
            remove-workbook-sheet!
            valid-sheet-name?
            unique-sheet-name
            workbook-watch-paths))

(define %primary-file "sheet.scm")
(define %cells-directory "cells")
(define %cell-suffix ".scm")
(define %format 1)

(define %workbook-file "workbook.scm")
(define %sheets-directory "sheets")
;; The sheet format is untouched by tabs, so sheet.scm still says 1.  What is
;; new is the document around it, and workbook.scm says 2 for the layout a
;; reader would need to understand to find the sheets at all.
(define %workbook-format 2)

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

(define (cell-file-path directory name)
  "The file cell NAME lives in inside DIRECTORY -- which may not exist yet, an
empty cell being a cell with no file.  Public because an external editor is
pointed straight at it."
  (cell-file directory name))


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

(define (make-directories! path)
  "Make PATH and every directory above it that is not there yet.  `mkdir' makes
one directory and fails if its parent is missing, which is no use for a path
like ~/.local/share/cellar/scratch where the whole chain may be new."
  (let loop ((parts (filter (lambda (part) (not (string-null? part)))
                            (string-split path #\/)))
             (so-far (if (string-prefix? "/" path) "" ".")))
    (unless (null? parts)
      (let ((next (string-append so-far "/" (car parts))))
        (ensure-directory! next)
        (loop (cdr parts) next))))
  path)


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

(define (save-cell! directory name source)
  "Write one cell to disk: SOURCE into its file, or no file at all when the
cell is empty.  This is how an edit reaches the disk -- a sheet is saved a cell
at a time, so the file for a cell is current the moment you finish typing it."
  (ensure-directory! directory)
  (ensure-directory! (cells-directory directory))
  (let ((file (cell-file directory name)))
    (if (or (not (string? source)) (string-null? (string-trim-both source)))
        (when (file-exists? file) (delete-file file))
        (write-cell directory name source))))

(define (write-sheet-metadata! directory rows columns widths)
  "Write what is true of the sheet rather than of any one cell."
  (ensure-directory! directory)
  (write-primary directory rows columns widths))

(define (write-primary directory rows columns widths)
  (write-if-changed (primary-file directory) (primary-text rows columns widths)))

(define (primary-text rows columns widths)
  ;; An entry to a line, so that changing the size of a sheet is a one-line
  ;; diff rather than a rewritten file.
  (call-with-output-string
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
  ;; A trailing newline: these are text files, and diffs of files without one
  ;; are a nuisance to read.
  (write-if-changed (cell-file directory name)
                    (if (string-suffix? "\n" source)
                        source
                        (string-append source "\n"))))

(define (write-if-changed path text)
  "Write TEXT to PATH, unless PATH already holds exactly that.

Worth the read it costs.  Cellar rewrites every cell of a sheet for a single
moved row, and most of those files are not changing; leaving them alone keeps
their mtimes still, keeps `git status' honest about what was edited, and --
since the sheet folder is watched -- stops Cellar waking itself up over its own
writes.  A save of a sheet where one cell changed should be one write."
  (unless (and (file-exists? path)
               (catch #t
                 (lambda ()
                   (string=? text (call-with-input-file path get-string-all)))
                 (lambda arguments #f)))
    (call-with-output-file path (lambda (port) (display text port)))))


;;;
;;; Reading
;;;

(define (read-sheet-cells directory)
  "Every cell on disk, as an alist of name to source, sorted by name so that
two readings of an unchanged directory compare equal."
  (sort (map (lambda (name) (cons name (read-cell directory name)))
             (stored-cell-names directory))
        (lambda (a b) (string<? (car a) (car b)))))

(define (read-sheet-metadata directory)
  "The size and column widths recorded for the sheet at DIRECTORY."
  (read-primary directory))

(define (load-sheet! sheet directory)
  "Read the sheet at DIRECTORY into SHEET, replacing what was there.  Returns
the column widths it was saved with, as an alist of column index to width."
  (unless (sheet-directory? directory)
    (throw 'cellar-store-error
           (format #f "~a is not a Cellar sheet" directory)))
  (let ((metadata (read-primary directory)))
    (alist->sheet! sheet (read-sheet-cells directory))
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


;;;
;;; Workbooks
;;;

;; A workbook is the folder the tab bar shows and the folder git holds: an
;; index saying which sheets there are and in what order, and a folder for each
;; of them under sheets/.  Everything above this line works on one sheet folder
;; and is reused unchanged, which is the point of keeping the two apart.

(define %default-sheet-name "Sheet 1")

(define (workbook-file directory)
  (string-append directory "/" %workbook-file))

(define (sheets-directory directory)
  (string-append directory "/" %sheets-directory))

(define (workbook-directory path)
  "The workbook directory PATH names.  A workbook can be pointed at by its own
folder, by its workbook.scm, or by the sheet.scm of any sheet inside it, and
all three arrive here as the folder."
  (cond
   ((or (not (file-exists? path)) (directory? path)) path)
   ((string=? (basename path) %workbook-file) (dirname path))
   ((string=? (basename path) %primary-file)
    (let* ((sheet (dirname path))
           (parent (dirname sheet)))
      ;; sheets/Q1/sheet.scm is two folders down from the workbook; the
      ;; sheet.scm of a workbook from before tabs is one.
      (if (string=? (basename parent) %sheets-directory) (dirname parent) sheet)))
   (else path)))

(define (workbook-directory? path)
  "Is PATH a Cellar workbook -- a folder with an index in it, or a single sheet
from before there were tabs?"
  (let ((directory (workbook-directory path)))
    (and (file-exists? directory)
         (directory? directory)
         (or (file-exists? (workbook-file directory))
             (sheet-directory? directory)))))

(define (workbook-format-1? directory)
  "Is DIRECTORY a workbook from before tabs -- one sheet lying at the top of the
folder, with no index above it?"
  (and (not (file-exists? (workbook-file directory)))
       (file-exists? (primary-file directory))))

(define (workbook-name directory)
  "What to call the workbook at DIRECTORY in a window title."
  (basename (workbook-directory directory)))

(define (bare-name directory)
  "The folder's name without the extension: budget.cellar -> budget."
  (let ((base (basename directory)))
    (if (string-suffix? ".cellar" base)
        (substring base 0 (- (string-length base) (string-length ".cellar")))
        base)))

(define (legacy-sheet-name directory)
  "What to call the one sheet of a workbook from before tabs.  Its folder is
the workbook's, so it has no name of its own and borrows the workbook's --
budget.cellar opens with a tab that says budget, which is what it was already
being called in the title bar."
  (let ((name (bare-name directory)))
    (if (valid-sheet-name? name) (string-trim-both name) %default-sheet-name)))


;;;
;;; Naming a sheet
;;;

(define (valid-sheet-name? name)
  "Can NAME be a sheet?  It becomes a folder name and is written into an index
that is read back with `read', so it has to be a name a folder can have: not
empty, not a path, and not hidden -- which rules out `.' and `..' along with
it."
  (and (string? name)
       (let ((name (string-trim-both name)))
         (and (not (string-null? name))
              (<= (string-length name) 64)
              (not (string-index name #\/))
              (not (string-index name #\nul))
              (not (char=? (string-ref name 0) #\.))
              #t))))

(define (taken? directory name)
  "Is there already a sheet called NAME?  Compared without regard to case,
because on a good many filesystems `Q1' and `q1' would be one folder."
  (let ((folded (string-downcase name)))
    (any (lambda (existing) (string=? folded (string-downcase existing)))
         (workbook-sheet-names directory))))

(define (unique-sheet-name directory base)
  "BASE, or BASE with a different number after it -- whichever the workbook does
not already have.  What the Add Sheet dialog suggests."
  (let ((base (if (valid-sheet-name? base)
                  (string-trim-both base)
                  %default-sheet-name)))
    (if (not (taken? directory base))
        base
        (let* ((split (split-trailing-number base))
               (stem (car split)))
          (let loop ((n (+ 1 (cdr split))))
            (let ((candidate (string-append stem (number->string n))))
              (if (taken? directory candidate) (loop (+ n 1)) candidate)))))))

(define (split-trailing-number name)
  "NAME split into what comes before a trailing number and the number itself,
so that the sheet after `Sheet 2' is `Sheet 3' and the one after `Q1' is `Q2'.

The split keeps whatever stood between the two exactly as it was -- a space in
the one case, nothing at all in the other -- so a suggested name is spelled the
way the name it was suggested from is.  A name with no number on the end is
given a space and a 1, which makes `Summary' into `Summary 2'."
  (let loop ((i (string-length name)))
    (cond
     ((and (> i 0) (char-numeric? (string-ref name (- i 1)))) (loop (- i 1)))
     ((or (= i (string-length name)) (= i 0)) (cons (string-append name " ") 1))
     (else
      (let ((n (string->number (substring name i))))
        (if n
            (cons (substring name 0 i) n)
            (cons (string-append name " ") 1)))))))


;;;
;;; Making one
;;;

(define (create-workbook! directory name git?)
  "Make a workbook at DIRECTORY holding one empty sheet called NAME, and a git
repository of the whole thing when GIT? is true.  Returns 'created, or
'created-without-git when git could not be run.  Throws if the folder cannot be
made or already holds a workbook.

The repository is made around the workbook rather than around the sheet, which
is the whole reason for this layer: several spreadsheets, one history."
  (when (workbook-directory? directory)
    (throw 'cellar-store-error
           (format #f "~a is already a Cellar workbook" directory)))
  (let ((name (if (valid-sheet-name? name)
                  (string-trim-both name)
                  %default-sheet-name)))
    (ensure-directory! directory)
    (ensure-directory! (sheets-directory directory))
    (create-sheet-directory! (string-append (sheets-directory directory) "/" name)
                             #f)
    (write-workbook-index! directory (list name) name)
    (if (and git? (not (git-init directory)))
        'created-without-git
        'created)))


;;;
;;; Reading the index
;;;

(define (read-workbook directory)
  "What the index says, as an alist.  A workbook from before tabs has no index
and answers as though it had one naming its single sheet."
  (let ((directory (workbook-directory directory)))
    (if (workbook-format-1? directory)
        (let ((name (legacy-sheet-name directory)))
          `((format . 1) (sheets ,name) (active . ,name)))
        (let ((datum (catch #t
                       (lambda ()
                         (call-with-input-file (workbook-file directory) read))
                       (lambda args #f))))
          (if (list? datum) datum '())))))

(define (workbook-sheet-names directory)
  "The sheets of the workbook at DIRECTORY, in the order the tabs should show
them.

What is on disk decides which sheets there are, and the index decides only
their order.  So a sheet that arrived in someone else's commit turns up as a
tab rather than being ignored, and one that a `git checkout' took away leaves
rather than being a tab over a folder that is not there.  That makes the index
a hint, which is the most that a file two people can edit at once should be."
  (let* ((directory (workbook-directory directory))
         (on-disk (stored-sheet-names directory))
         (listed (filter (lambda (name) (member name on-disk))
                         (index-sheet-names (read-workbook directory)))))
    (append listed
            (sort (filter (lambda (name) (not (member name listed))) on-disk)
                  string<?))))

(define (index-sheet-names index)
  (let ((listed (assq-ref index 'sheets)))
    (if (list? listed) (filter valid-sheet-name? listed) '())))

(define (stored-sheet-names directory)
  "The sheets that actually have a folder with a sheet in it, by name."
  (if (workbook-format-1? directory)
      (list (legacy-sheet-name directory))
      (let ((sheets (sheets-directory directory)))
        (filter (lambda (name)
                  (sheet-directory? (string-append sheets "/" name)))
                (sheet-folder-names directory)))))

(define (sheet-folder-names directory)
  "Every folder directly under sheets/, whether or not there is a sheet in it
yet."
  (if (workbook-format-1? directory)
      '()
      (let ((sheets (sheets-directory directory)))
        (if (not (file-exists? sheets))
            '()
            (filter (lambda (name)
                      (and (valid-sheet-name? name)
                           (directory? (string-append sheets "/" name))))
                    (or (scandir sheets (lambda (name) #t)) '()))))))

(define (workbook-active-sheet directory)
  "The sheet whose tab was showing when the workbook was last written, or the
first one when that sheet is no longer there."
  (let ((names (workbook-sheet-names directory))
        (active (assq-ref (read-workbook directory) 'active)))
    (cond ((and (string? active) (member active names)) active)
          ((pair? names) (car names))
          (else #f))))

(define (workbook-sheet-directory directory name)
  "The folder sheet NAME lives in.  In a workbook from before tabs that is the
workbook's own folder, which is exactly what makes one readable where it lies."
  (let ((directory (workbook-directory directory)))
    (if (workbook-format-1? directory)
        directory
        (string-append (sheets-directory directory) "/" name))))

(define (workbook-watch-paths directory)
  "Every path that has to be watched for the workbook to notice a change to
itself: the index, the folder the sheets are in -- so that a sheet appearing or
disappearing is seen -- every folder in it, and each sheet's own cells and
primary file.

A folder under sheets/ is watched whether or not there is a sheet in it yet.  A
sheet arriving in a `git checkout' is a folder that appears and is filled in a
moment afterwards, and watching only the folders that are already sheets would
mean hearing about that one while it was still empty and never hearing about it
again."
  (let* ((directory (workbook-directory directory))
         (sheets (sheets-directory directory)))
    (cons* (workbook-file directory)
           sheets
           (append (map (lambda (name) (string-append sheets "/" name))
                        (sheet-folder-names directory))
                   (append-map
                    (lambda (name)
                      (let ((sheet (workbook-sheet-directory directory name)))
                        (list (cells-directory sheet) (primary-file sheet))))
                    (workbook-sheet-names directory))))))


;;;
;;; Writing the index
;;;

(define (write-workbook-index! directory names active)
  "Write which sheets there are and which one is showing."
  (let ((directory (workbook-directory directory)))
    (ensure-directory! directory)
    (write-if-changed (workbook-file directory) (workbook-text names active))))

(define (workbook-text names active)
  ;; A line to an entry and a line to a sheet, so that adding a sheet, renaming
  ;; one or dragging a tab is a one-line diff rather than a rewritten file.
  (call-with-output-string
    (lambda (port)
      (display ";; A Cellar workbook. Each sheet is a folder under sheets/.\n"
               port)
      (format port "((format . ~a)\n" %workbook-format)
      (display " (sheets" port)
      (for-each (lambda (name) (format port "\n  ~s" name)) names)
      (display ")\n" port)
      (format port " (active . ~s))\n" (or active "")))))

(define (set-workbook-active! directory name)
  "Remember which tab was showing.  A workbook from before tabs has one sheet
and no index to write this into, and does not miss it."
  (let ((directory (workbook-directory directory)))
    (unless (workbook-format-1? directory)
      (write-workbook-index! directory (workbook-sheet-names directory) name))))

(define (set-workbook-order! directory names)
  "Remember the order the tabs are in."
  (let ((directory (workbook-directory directory)))
    (unless (workbook-format-1? directory)
      (write-workbook-index! directory names
                             (workbook-active-sheet directory)))))


;;;
;;; Adding, renaming and removing a sheet
;;;

(define (migrate-workbook! directory)
  "Move a workbook from before tabs into sheets/, so that it can hold a second
sheet.  Returns the name its one sheet now has.

The folder is renamed, not copied, so that git sees a rename rather than a
deletion and an unrelated new file, and `git log --follow' still walks back
through a cell's history.

Nothing calls this until a second sheet is actually asked for.  A single sheet
is perfectly readable where it lies, and rearranging somebody's repository on
the way to merely opening it would be a rude way to say hello."
  (let ((name (legacy-sheet-name directory)))
    (ensure-directory! (sheets-directory directory))
    (let ((target (string-append (sheets-directory directory) "/" name)))
      (ensure-directory! target)
      (rename-file (primary-file directory)
                   (string-append target "/" %primary-file))
      (when (file-exists? (cells-directory directory))
        (rename-file (cells-directory directory)
                     (string-append target "/" %cells-directory))))
    (write-workbook-index! directory (list name) name)
    name))

(define (add-workbook-sheet! directory name)
  "Add an empty sheet called NAME to the workbook, and return the name it was
given.  Throws if NAME is not a name a folder can have, or is already taken."
  (let* ((directory (workbook-directory directory))
         (name (and (valid-sheet-name? name) (string-trim-both name))))
    (unless name
      (throw 'cellar-store-error
             "A sheet needs a name, and not one with a / in it"))
    (when (workbook-format-1? directory)
      (migrate-workbook! directory))
    (when (taken? directory name)
      (throw 'cellar-store-error
             (format #f "This workbook already has a sheet called ~a" name)))
    ;; Read the order before the folder exists, or the new sheet would be found
    ;; on disk and appended to the index twice.
    (let ((existing (workbook-sheet-names directory)))
      (ensure-directory! (sheets-directory directory))
      (create-sheet-directory! (workbook-sheet-directory directory name) #f)
      (write-workbook-index! directory (append existing (list name)) name)
      name)))

(define (rename-workbook-sheet! directory old new)
  "Rename a sheet, folder and all.  Returns the name it now has."
  (let* ((directory (workbook-directory directory))
         (new (and (valid-sheet-name? new) (string-trim-both new))))
    (unless new
      (throw 'cellar-store-error
             "A sheet needs a name, and not one with a / in it"))
    (if (string=? old new)
        new
        (begin
          (when (workbook-format-1? directory)
            (migrate-workbook! directory))
          (when (and (taken? directory new) (not (string-ci=? old new)))
            (throw 'cellar-store-error
                   (format #f "This workbook already has a sheet called ~a" new)))
          (unless (member old (workbook-sheet-names directory))
            (throw 'cellar-store-error
                   (format #f "There is no sheet called ~a" old)))
          ;; Both worked out before the folder moves, since neither can be read
          ;; back afterwards under the name it was asked about.
          (let ((names (map (lambda (name) (if (string=? name old) new name))
                            (workbook-sheet-names directory)))
                (active (workbook-active-sheet directory)))
            (rename-file (workbook-sheet-directory directory old)
                         (workbook-sheet-directory directory new))
            (write-workbook-index! directory names
                                   (if (equal? active old) new active))
            new)))))

(define (remove-workbook-sheet! directory name)
  "Delete a sheet, and return the sheets that are left.  Refuses to delete the
last one: a workbook with nothing in it is not something the rest of the
program can show."
  (let* ((directory (workbook-directory directory))
         (names (workbook-sheet-names directory)))
    (unless (member name names)
      (throw 'cellar-store-error (format #f "There is no sheet called ~a" name)))
    (when (<= (length names) 1)
      (throw 'cellar-store-error "A workbook has to keep at least one sheet"))
    (let ((remaining (filter (lambda (existing) (not (string=? existing name)))
                             names))
          (active (workbook-active-sheet directory)))
      (remove-sheet-files! (workbook-sheet-directory directory name))
      (write-workbook-index! directory remaining
                             (if (equal? active name) (car remaining) active))
      remaining)))

(define (remove-sheet-files! directory)
  "Delete the files a sheet is made of, and the folders that held them if
nothing else is left in them.

Only what Cellar wrote is deleted.  A sheet folder is a place a person can keep
a README, and a note left in one is a reason to leave the folder standing --
empty of a sheet, and so no longer a tab -- rather than to take the note down
with it."
  (let ((cells (cells-directory directory)))
    (when (file-exists? cells)
      (for-each (lambda (name) (delete-file (cell-file directory name)))
                (stored-cell-names directory))
      (try-rmdir cells)))
  (when (file-exists? (primary-file directory))
    (delete-file (primary-file directory)))
  (try-rmdir directory))

(define (try-rmdir directory)
  "Remove DIRECTORY if it is empty, and shrug if it is not."
  (catch #t
    (lambda () (rmdir directory) #t)
    (lambda arguments #f)))
