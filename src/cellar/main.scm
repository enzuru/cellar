;;; Cellar -- application entry point.

(define-module (cellar main)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar model)
  #:use-module (cellar grid)
  #:use-module (cellar store)
  #:use-module (cellar editor)
  #:use-module (cellar external)
  #:use-module (cellar config)
  #:use-module (cellar preferences)
  #:use-module (cellar watch)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (main))

(define %rows 100)
(define %columns 26)
(define %application-id "dev.enzuru.Cellar")

(define %css "
/* Monospace throughout the grid: digits that line up are the whole point of a
   column of numbers, and a cell holds Scheme, which reads as code everywhere
   else in the program too. */
.cellar-cell, .cellar-gutter {
  font-family: monospace;
  padding: 2px 6px;
  border-radius: 4px;
}
.cellar-gutter {
  opacity: 0.55;
  font-size: 0.85em;
}
.cellar-cell.cellar-active {
  background-color: alpha(currentColor, 0.10);
  box-shadow: inset 0 0 0 2px @accent_bg_color;
  font-weight: bold;
}
.cellar-cell.cellar-error {
  color: @error_color;
}
/* A row or column being dragged, and the place it would land. */
.cellar-cell.cellar-drag-source, .cellar-gutter.cellar-drag-source {
  opacity: 0.35;
}
.cellar-cell.cellar-drag-target, .cellar-gutter.cellar-drag-target {
  background-color: alpha(@accent_bg_color, 0.30);
  box-shadow: inset 0 0 0 1px @accent_bg_color;
}
columnview.data-table > header > button.cellar-drag-source {
  opacity: 0.5;
}
columnview.data-table > header > button.cellar-drag-target {
  background-color: alpha(@accent_bg_color, 0.30);
}
columnview.data-table > header > button {
  font-family: monospace;
  font-weight: bold;
}
")

;; The folder of the sheet being edited, or #f when no sheet is open.  A sheet
;; always has one now: every edit is written to it as it is made, so there is no
;; such thing as a sheet that is not on disk.
(define *path* #f)

;; True when that folder is one Cellar made to hold a scratch sheet, which
;; changes nothing but what the title bar calls it.
(define *scratch?* #f)

;; The GFileMonitor over the open sheet, or #f.
(define *watcher* #f)


;;;
;;; Startup
;;;

(define (main arguments)
  (let ((file (match (cdr arguments)
                (() #f)
                ((path . _) path))))
    (let ((application (make <adw-application>
                         #:application-id %application-id)))
      (connect application 'activate
               (lambda (application) (activate application file)))
      (exit (run application '())))))

(define (activate application file)
  ;; GtkSourceView registers its GTypes here; without this the builder cannot
  ;; instantiate the GtkSourceView in editor.ui.
  (gtk-source-init)
  (load-config!)

  (let* ((ui-directory (find-ui-directory))
         (builder (make <gtk-builder>))
         (path (string-append ui-directory "/cellar.ui")))
    (when (eqv? 0 (add-from-file builder path))
      (error "cellar: could not load the main UI:" path))

    (let* ((window (get-object builder "main_window"))
           (window-title (get-object builder "window_title"))
           (reference-label (get-object builder "reference_label"))
           (source-label (get-object builder "source_label"))
           (edit-button (get-object builder "edit_button"))
           (recalculate-button (get-object builder "recalculate_button"))
           (toast-overlay (get-object builder "toast_overlay"))
           (sheet-view (get-object builder "sheet_view"))
           (line-menu (get-object builder "line_menu"))
           (cell-bar (get-object builder "cell_bar"))
           (main-stack (get-object builder "main_stack"))
           (new-sheet-dialog (get-object builder "new_sheet_dialog"))
           (new-sheet-name (get-object builder "new_sheet_name"))
           (new-sheet-location (get-object builder "new_sheet_location"))
           (new-sheet-location-label
            (get-object builder "new_sheet_location_label"))
           (new-sheet-git (get-object builder "new_sheet_git"))
           (sheet (make-sheet %rows %columns))
           (grid #f)
           ;; Where the New Sheet dialog would put a sheet, until told otherwise.
           (location (default-location)))

      (define (notify message)
        (add-toast toast-overlay (make <adw-toast> #:title message)))

      (define (sheet-showing?)
        (equal? "sheet" (get-visible-child-name main-stack)))

      (define (retitle)
        (set-subtitle window-title
                      (cond (*scratch?* "Scratch")
                            (*path* (sheet-name *path*))
                            (else "No sheet open"))))

      (define (show-start-page)
        (set-visible-child-name main-stack "start")
        ;; The cell bar and the recalculate button speak about a sheet; with no
        ;; sheet open there is nothing for them to say.
        (set-visible cell-bar #f)
        (set-visible recalculate-button #f)
        (retitle))

      (define (show-sheet-page)
        (set-visible-child-name main-stack "sheet")
        (set-visible cell-bar #t)
        (set-visible recalculate-button #t)
        (retitle)
        (show-selection (grid-active grid))
        (grid-focus! grid))

      (define (show-selection r)
        (set-label reference-label (ref->name r))
        (let ((source (cell-source sheet r)))
          (if source
              (begin
                (set-label source-label (one-line source))
                (remove-css-class source-label "dim-label"))
              (begin
                (set-label source-label
                           "empty — double-click a cell to write Guile")
                (add-css-class source-label "dim-label")))))

      ;;
      ;; Saving.  There is no command for it: a cell is written the moment it
      ;; is changed, so the folder on disk is what the sheet is rather than a
      ;; copy of it taken when you remembered to ask.
      ;;

      (define (persist-cell! r)
        "Write one cell to its own file."
        (when *path*
          (catch #t
            (lambda () (save-cell! *path* (ref->name r) (cell-source sheet r)))
            (lambda (key . args)
              (report-failure (format #f "save ~a" (ref->name r)) key args)))))

      (define (persist-layout!)
        "Write the size of the sheet and its column widths."
        (when (and *path* grid)
          (catch #t
            (lambda ()
              (write-sheet-metadata! *path* (sheet-rows sheet)
                                     (sheet-columns sheet)
                                     (grid-column-widths grid)))
            (lambda (key . args) (report-failure "save the sheet" key args)))))

      (define (persist-cells!)
        "Write every cell.  Moving a row or inserting a column renames the files
of every cell it shifted, so the cheapest correct answer for those is to write
the lot -- it is a few dozen small files, and it deletes the ones left behind."
        (when (and *path* grid)
          (catch #t
            (lambda ()
              (save-sheet! sheet *path* (grid-column-widths grid)))
            (lambda (key . args) (report-failure "save the sheet" key args)))))

      (define (on-grid-change what)
        (if (eq? what 'layout) (persist-layout!) (persist-cells!)))

      (define (apply-edit r)
        (lambda (text)
          (set-cell-source! sheet r text)
          (persist-cell! r)
          (grid-refresh! grid)
          (show-selection r)))

      (define (edit-internally r)
        (open-cell-editor ui-directory window sheet r (apply-edit r)))

      (define (edit-cell r)
        "Open the cell wherever the preferences say. If the external editor
cannot be started we say so and fall back, rather than leaving a cell that
cannot be edited at all."
        (let ((command (effective-editor-command)))
          (if (and command *path*)
              (if (open-external-editor command *path* r notify)
                  (notify (format #f "Editing ~a in ~a"
                                  (ref->name r) (external-editor-name command)))
                  (edit-internally r))
              (edit-internally r))))

      ;;
      ;; Catching up with the disk
      ;;

      (define (rewatch!)
        "Watch the open sheet, and only it."
        (unwatch! *watcher*)
        (set! *watcher*
              (and *path* (watch-sheet! *path* reload-from-disk))))

      (define (reload-from-disk)
        "Something under the sheet folder changed; take the folder as the truth.

Cellar's own writes come through here too, and have to be harmless when they
do -- which they are, because by the time the file lands the model already says
what the file says, and the comparison below finds nothing to do.  That is the
whole reason this compares rather than tries to remember which files were ours."
        (when (and *path* grid (sheet-directory? *path*))
          (catch #t
            (lambda ()
              (let ((on-disk (read-sheet-cells *path*)))
                (unless (equal? on-disk (cells-by-name sheet))
                  (let* ((metadata (read-sheet-metadata *path*))
                         (active (grid-active grid)))
                    (alist->sheet! sheet on-disk)
                    (grow-sheet! sheet
                                 (or (assq-ref metadata 'rows) 0)
                                 (or (assq-ref metadata 'columns) 0))
                    (grid-sync-size! grid)
                    ;; Keep the cursor where the user left it, unless the sheet
                    ;; shrank out from under it.
                    (grid-set-active! grid
                                      (if (valid-ref? sheet active)
                                          active
                                          (make-ref 0 0)))
                    (grid-refresh! grid)
                    (show-selection (grid-active grid))
                    (notify "Reloaded — the sheet changed on disk")))))
            (lambda (key . args)
              (report-failure "read the sheet" key args)))))

      (define (cells-by-name sheet)
        "The model's cells in the order `read-sheet-cells' gives the disk's, so
that the two can simply be compared.  `sheet->alist' is in row-major order and
the disk's is sorted by name; sorting both the same way is what makes Cellar's
own writes come back through the watcher as no-ops."
        (sort (sheet->alist sheet)
              (lambda (a b) (string<? (car a) (car b)))))

      (define (report-failure what key args)
        (notify (if (and (eq? key 'cellar-store-error) (pair? args))
                    (car args)
                    (format #f "Could not ~a" what))))


      ;;
      ;; Opening, making and saving
      ;;

      ;; Which of the two questions the New Sheet dialog is currently asking.
      (define *copying?* #f)

      (define (empty-sheet!)
        (alist->sheet! sheet '())
        (grow-sheet! sheet %rows %columns)
        (grid-sync-size! grid)
        (grid-set-active! grid (make-ref 0 0))
        (grid-refresh! grid))

      (define (open-sheet path)
        "Open the sheet PATH names, by its directory or by its primary file."
        (let ((directory (sheet-directory path)))
          (if (not (sheet-directory? directory))
              (begin
                (notify (format #f "~a is not a Cellar sheet" (basename directory)))
                #f)
              (catch #t
                (lambda ()
                  (let ((widths (load-sheet! sheet directory)))
                    (set! *path* directory)
                    (set! *scratch?* #f)
                    (grid-sync-size! grid)
                    (grid-set-column-widths! grid widths)
                    (grid-set-active! grid (make-ref 0 0))
                    (grid-refresh! grid)
                    (rewatch!)
                    (show-sheet-page)
                    #t))
                (lambda (key . args)
                  (report-failure (format #f "open ~a" (basename directory))
                                  key args)
                  #f)))))

      (define (scratch-sheet)
        "A sheet to think in.  It still lives in a folder -- everything does now
-- but one Cellar picks, out of the way under the data directory, so that
starting one asks nothing.  Copy To puts it somewhere you chose."
        (catch #t
          (lambda ()
            (let ((directory (scratch-location)))
              (create-sheet-directory! directory #f)
              (empty-sheet!)
              (set! *path* directory)
              (set! *scratch?* #t)
              (persist-layout!)
              (rewatch!)
              (show-sheet-page)))
          (lambda (key . args)
            (report-failure "make a scratch sheet" key args))))

      (define (copy-to directory)
        "Write the sheet to a folder of its own and carry on editing it there.
The sheet you were in is left exactly as it was."
        (catch #t
          (lambda ()
            (save-sheet! sheet directory (grid-column-widths grid))
            (set! *path* directory)
            (set! *scratch?* #f)
            (rewatch!)
            (retitle)
            (notify (format #f "Now editing ~a" (sheet-name directory))))
          (lambda (key . args)
            (report-failure (format #f "copy to ~a" (basename directory))
                            key args))))

      (define (ask-for-new-sheet suggestion copying?)
        "The same dialog for New Sheet and for Copy To: both are a name and a
place to put it, and COPYING? decides which of the two the answer means."
        (set! *copying?* copying?)
        (set-heading new-sheet-dialog (if copying? "Copy Sheet To" "New Sheet"))
        (set-body new-sheet-dialog
                  (if copying?
                      "The sheet is written to a new folder, and that is the one you carry on editing. The folder you were in is left as it stands."
                      "A sheet is a folder: one small file for every cell, and a primary file for the sheet itself."))
        (adw-alert-dialog-set-response-label new-sheet-dialog "create"
                                             (if copying? "Copy" "Create"))
        (gtk-editable-set-text new-sheet-name suggestion)
        (set-label new-sheet-location-label location)
        (present new-sheet-dialog window))

      (define (create-new-sheet)
        (let* ((typed (string-trim-both (gtk-editable-get-text new-sheet-name)))
               (name (if (string-null? typed) "sheet" typed))
               (directory (string-append location "/" (sheet-folder-name name))))
          (catch #t
            (lambda ()
              (let ((made (create-sheet-directory! directory
                                                   (get-active new-sheet-git))))
                (if *copying?*
                    ;; copy-to writes the sheet and says where you now are.
                    (copy-to directory)
                    (begin
                      (empty-sheet!)
                      (set! *path* directory)
                      (set! *scratch?* #f)
                      (save-sheet! sheet directory (grid-column-widths grid))
                      (rewatch!)
                      (notify (format #f "Created ~a" (sheet-name directory)))))
                (when (eq? made 'created-without-git)
                  (notify "The folder was made, but git could not be run"))
                (show-sheet-page)))
            (lambda (key . args)
              (report-failure (format #f "create ~a" (basename directory))
                              key args)))))


      ;;
      ;; Actions
      ;;

      ;; Named define-action, not add-action: a local `add-action' would shadow
      ;; the GActionMap generic of the same name that it needs to call.
      (define (define-action name accelerators procedure)
        (let ((action (make <g-simple-action> #:name name)))
          (connect action 'activate (lambda (action parameter) (procedure)))
          (g-action-map-add-action application action)
          (unless (null? accelerators)
            (set-accels-for-action application
                                   (string-append "app." name)
                                   accelerators))))

      ;; An action that means nothing with no sheet on screen, and does
      ;; nothing there: the shortcuts are live whichever page is showing.
      (define (on-sheet procedure)
        (lambda () (when (sheet-showing?) (procedure))))

      (define (install-actions)
        (define-action "new" '("<Control>n")
                    (lambda () (ask-for-new-sheet "sheet" #f)))

        (define-action "new-scratch" '("<Control><Shift>n")
                    (lambda () (scratch-sheet)))

        (define-action "open" '("<Control>o")
                    (lambda ()
                      (choose-folder window (lambda (path) (open-sheet path)))))

        ;; There is nothing to save: the sheet on disk is already this one.
        ;; Ctrl+S is too deep a reflex to leave doing nothing silently, so it
        ;; says so instead.
        (define-action "save" '("<Control>s")
                    (on-sheet
                     (lambda () (notify "Cellar saves each cell as you edit it"))))

        (define-action "copy-to" '("<Control><Shift>s")
                    (on-sheet
                     (lambda ()
                       (ask-for-new-sheet
                        (if (and *path* (not *scratch?*)) (sheet-name *path*) "sheet")
                        #t))))

        (define-action "recalculate" '("<Control>r")
                    (on-sheet
                     (lambda ()
                       (invalidate-sheet! sheet)
                       (grid-refresh! grid)
                       (notify "Recalculated"))))

        (define-action "clear-cell" '("Delete")
                    (on-sheet
                     (lambda ()
                       (let ((r (grid-active grid)))
                         (set-cell-source! sheet r "")
                         (persist-cell! r)
                         (grid-refresh! grid)
                         (show-selection r)))))

        (define-action "edit-cell" '("<Control>e")
                    (on-sheet (lambda () (edit-cell (grid-active grid)))))

        ;; Reordering. The model rewrites references as it moves cells, so a
        ;; sheet means the same thing after a move as it did before; all that
        ;; changes is where you read it.
        (define (move-line axis delta)
          (on-sheet
           (lambda ()
             (unless (grid-move-line! grid axis delta)
               (notify (if (eq? axis 'row)
                           "The row is already at the edge of the sheet"
                           "The column is already at the edge of the sheet"))))))

        (define-action "move-row-up" '("<Control><Shift>Up") (move-line 'row -1))
        (define-action "move-row-down" '("<Control><Shift>Down") (move-line 'row 1))
        (define-action "move-column-left" '("<Control><Shift>Left")
                    (move-line 'column -1))
        (define-action "move-column-right" '("<Control><Shift>Right")
                    (move-line 'column 1))

        ;; Inserting, which is the same machinery seen from the other side: the
        ;; cells below or to the right shift along, and every reference to them
        ;; shifts too.
        (define (insert-line axis where)
          (on-sheet
           (lambda ()
             (when (grid-insert-line! grid axis where)
               (notify (if (eq? axis 'row) "Row inserted" "Column inserted"))))))

        (define-action "insert-row-before" '("<Control><Alt>Up")
                    (insert-line 'row 'before))
        (define-action "insert-row-after" '("<Control><Alt>Down")
                    (insert-line 'row 'after))
        (define-action "insert-column-before" '("<Control><Alt>Left")
                    (insert-line 'column 'before))
        (define-action "insert-column-after" '("<Control><Alt>Right")
                    (insert-line 'column 'after))

        (define-action "preferences" '("<Control>comma")
                    (lambda () (open-preferences ui-directory window)))

        (define-action "shortcuts" '("<Control>question")
                    (lambda () (show-shortcuts window)))

        (define-action "about" '()
                    (lambda () (show-about window)))

        (define-action "quit" '("<Control>q")
                    (lambda () (gtk-window-close window))))

      (install-css)
      (install-icons)
      ;; Wayland matches the window to its .desktop file by application id and
      ;; ignores this; X11 has no such association and needs to be told.
      (set-icon-name window %application-id)
      (set! grid (make-grid sheet-view sheet line-menu show-selection edit-cell
                             on-grid-change))

      (connect edit-button 'clicked
               (lambda (button) (edit-cell (grid-active grid))))
      (connect recalculate-button 'clicked
               (lambda (button)
                 (invalidate-sheet! sheet)
                 (grid-refresh! grid)
                 (notify "Recalculated")))
      (connect new-sheet-location 'clicked
               (lambda (button)
                 (choose-folder window
                                (lambda (path)
                                  (set! location path)
                                  (set-label new-sheet-location-label path)))))
      (connect new-sheet-dialog 'response
               (lambda (dialog response)
                 (when (equal? response "create")
                   (create-new-sheet))))

      (install-actions)
      (show-start-page)

      (add-window application window)
      (present window)

      ;; A sheet named on the command line opens straight away; without one the
      ;; start page asks what to open, since a sheet now has to live somewhere.
      (when file
        (unless (open-sheet file)
          (show-start-page))))))

(define (default-location)
  (or (getenv "HOME") (getcwd)))

(define (scratch-location)
  "A folder of its own for a scratch sheet, under the XDG data directory and
named for the moment it was started, so two of them never collide."
  (let* ((data (or (getenv "XDG_DATA_HOME")
                   (string-append (or (getenv "HOME") (getcwd)) "/.local/share")))
         (scratches (make-directories! (string-append data "/cellar/scratch"))))
    (let loop ((stamp (strftime "%Y-%m-%d-%H%M%S" (localtime (current-time))))
               (n 0))
      (let ((candidate (string-append scratches "/"
                                      (if (zero? n)
                                          stamp
                                          (format #f "~a-~a" stamp n))
                                      ".cellar")))
        (if (file-exists? candidate)
            (loop stamp (+ n 1))
            candidate)))))

(define (sheet-folder-name name)
  "A sheet's folder is named like a file would be: budget -> budget.cellar."
  (if (string-suffix? ".cellar" name)
      name
      (string-append name ".cellar")))

(define (external-editor-name command)
  "The bare program name out of COMMAND, for a toast that reads as a sentence."
  (let ((argv (editor-argv command "")))
    (if (null? argv) command (basename (car argv)))))

(define (one-line text)
  "Collapse TEXT onto a single line for the cell bar."
  (let ((flat (string-map (lambda (c) (if (char=? c #\newline) #\space c)) text)))
    (let loop ((s flat))
      (if (string-contains s "  ")
          (loop (string-replace-substring s "  " " "))
          (string-trim-both s)))))

(define (find-ui-directory)
  "Locate the .ui files, whether running from the source tree or installed."
  (define (ui-in directory)
    (and directory
         (let ((candidate (string-append directory "/ui")))
           (and (file-exists? (string-append candidate "/cellar.ui"))
                candidate))))
  (or (getenv "CELLAR_UI_DIR")
      ;; src/cellar/main.scm -> src/cellar -> src -> the project root.
      (let ((module-file (%search-load-path "cellar/main.scm")))
        (and module-file
             (ui-in (dirname (dirname (dirname module-file))))))
      (ui-in (getcwd))
      (error "cellar: cannot find the ui directory; set CELLAR_UI_DIR")))

(define (find-icon-directory)
  "Locate the bundled icon theme when running from the source tree.

Installed, the icons land in a share/icons that XDG_DATA_DIRS already covers,
so #f here is the normal answer and not an error."
  (define (icons-in directory)
    (and directory
         (let ((candidate (string-append directory "/data/icons")))
           (and (file-exists? (string-append candidate "/hicolor/scalable/apps/"
                                             %application-id ".svg"))
                candidate))))
  (or (getenv "CELLAR_ICON_DIR")
      ;; src/cellar/main.scm -> src/cellar -> src -> the project root.
      (let ((module-file (%search-load-path "cellar/main.scm")))
        (and module-file
             (icons-in (dirname (dirname (dirname module-file))))))
      (icons-in (getcwd))))

(define (install-icons)
  (let ((directory (find-icon-directory)))
    (when directory
      (add-search-path (gtk-icon-theme-get-for-display (gdk-display-get-default))
                       directory))))

(define (install-css)
  (let ((provider (make <gtk-css-provider>)))
    (load-from-string provider %css)
    (gtk-style-context-add-provider-for-display
     (gdk-display-get-default) provider 600)))


;;;
;;; Files
;;;

(define (chosen-file finish dialog result)
  "Run FINISH on an async file-dialog result, returning the chosen path or #f.
Cancelling raises a GError, which is the one error we expect and ignore --
anything the caller does with the path is deliberately left outside this
catch, so real failures are not silently swallowed."
  (let ((file (catch #t
                (lambda () (finish dialog result))
                (lambda args #f))))
    (and file (get-path file))))

(define (choose-folder window continue)
  "Ask for a folder, and call CONTINUE with its path.

A sheet is a folder, so this is the one chooser the application needs: opening
one picks the folder, and making one picks the folder to make it in."
  (let ((dialog (make <gtk-file-dialog> #:title "Choose a Folder")))
    (gtk-file-dialog-select-folder
     dialog window #f
     (lambda (dialog result data)
       (let ((path (chosen-file gtk-file-dialog-select-folder-finish
                                dialog result)))
         (when path (continue path))))
     #f)))


;;;
;;; Dialogs
;;;

(define (show-about window)
  (let ((dialog (make <adw-about-dialog>
                  #:application-name "Cellar"
                  #:application-icon %application-id
                  #:version "0.1.0"
                  #:developer-name "Written with GNU Guile, G-Golf and Blueprint"
                  #:comments "A spreadsheet with no formula language. Every cell is a Guile expression, and references like A1 are just variables you can use in it."
                  #:license-type 'gpl-3-0
                  #:website "https://www.gnu.org/software/g-golf/")))
    (present dialog window)))

(define %shortcuts
  '(("Arrow keys / Tab" . "Move the active cell")
    ("Double-click / Enter" . "Edit the active cell's Guile source")
    ("Ctrl+Return" . "Apply, while in the editor")
    ("Delete" . "Clear the active cell")
    ("Ctrl+Shift+Up / Down" . "Move the active row up or down")
    ("Ctrl+Shift+Left / Right" . "Move the active column left or right")
    ("Ctrl+Alt+Up / Down" . "Insert a row before or after the active one")
    ("Ctrl+Alt+Left / Right" . "Insert a column before or after the active one")
    ("Right-click a row number or a column header" . "The same four inserts")
    ("Ctrl+R" . "Recalculate the sheet")
    ("Ctrl+N / Ctrl+Shift+N" . "New sheet / New scratch sheet")
    ("Ctrl+O" . "Open a sheet folder")
    ("Ctrl+Shift+S" . "Copy this sheet to another folder")
    ("Ctrl+," . "Preferences")
    ("Ctrl+Q" . "Quit")))

(define (show-shortcuts window)
  (let ((dialog (make <adw-alert-dialog>
                  #:heading "Keyboard Shortcuts"
                  #:body (string-join
                          (map (lambda (entry)
                                 (format #f "~a — ~a" (car entry) (cdr entry)))
                               %shortcuts)
                          "\n"))))
    (add-response dialog "close" "Close")
    (present dialog window)))
