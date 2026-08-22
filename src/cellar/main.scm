;;; Cellar -- application entry point.

(define-module (cellar main)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar model)
  #:use-module (cellar grid)
  #:use-module (cellar editor)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (main))

(define %rows 100)
(define %columns 26)
(define %application-id "dev.enzuru.Cellar")

(define %css "
.cellar-cell, .cellar-gutter {
  padding: 2px 0;
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
  font-weight: bold;
}
")

;; The path of the sheet being edited, or #f for an unsaved one. The only
;; mutable application state that is not the sheet itself.
(define *path* #f)


;;;
;;; Startup
;;;

(define (main arguments)
  (let ((file (match (cdr arguments)
                (() #f)
                ((path . _) path))))
    (set! *path* file)
    (let ((application (make <adw-application>
                         #:application-id %application-id)))
      (connect application 'activate
               (lambda (application) (activate application file)))
      (exit (run application '())))))

(define (activate application file)
  ;; GtkSourceView registers its GTypes here; without this the builder cannot
  ;; instantiate the GtkSourceView in editor.ui.
  (gtk-source-init)

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
           (sheet (make-sheet %rows %columns))
           (grid #f))

      (define (notify message)
        (add-toast toast-overlay (make <adw-toast> #:title message)))

      (define (retitle)
        (set-subtitle window-title
                      (if *path* (basename *path*) "Untitled")))

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

      (define (edit-cell r)
        (open-cell-editor ui-directory window sheet r
                          (lambda (text)
                            (set-cell-source! sheet r text)
                            (grid-refresh! grid)
                            (show-selection r))))

      (install-css)
      (install-icons)
      ;; Wayland matches the window to its .desktop file by application id and
      ;; ignores this; X11 has no such association and needs to be told.
      (set-icon-name window %application-id)
      (set! grid (make-grid sheet-view sheet show-selection edit-cell))

      (connect edit-button 'clicked
               (lambda (button) (edit-cell (grid-active grid))))
      (connect recalculate-button 'clicked
               (lambda (button)
                 (invalidate-sheet! sheet)
                 (grid-refresh! grid)
                 (notify "Recalculated")))

      (install-actions application window sheet grid notify retitle
                       show-selection edit-cell)

      (when file
        (if (file-exists? file)
            (begin (load-sheet! sheet file)
                   (grid-refresh! grid))
            (notify (format #f "~a does not exist yet" (basename file)))))

      (retitle)
      (show-selection (grid-active grid))
      (add-window application window)
      (present window)
      (grid-focus! grid))))

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
;;; Actions
;;;

(define (install-actions application window sheet grid notify retitle
                         show-selection edit-cell)
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

  (define-action "recalculate" '("<Control>r")
              (lambda ()
                (invalidate-sheet! sheet)
                (grid-refresh! grid)
                (notify "Recalculated")))

  (define-action "clear-cell" '("Delete")
              (lambda ()
                (let ((r (grid-active grid)))
                  (set-cell-source! sheet r "")
                  (grid-refresh! grid)
                  (show-selection r))))

  (define-action "edit-cell" '("<Control>e")
              (lambda () (edit-cell (grid-active grid))))

  ;; Reordering. The model rewrites references as it moves cells, so a sheet
  ;; means the same thing after a move as it did before; all that changes is
  ;; where you read it.
  (define (move-line axis delta)
    (lambda ()
      (unless (grid-move-line! grid axis delta)
        (notify (if (eq? axis 'row)
                    "The row is already at the edge of the sheet"
                    "The column is already at the edge of the sheet")))))

  (define-action "move-row-up" '("<Control><Shift>Up") (move-line 'row -1))
  (define-action "move-row-down" '("<Control><Shift>Down") (move-line 'row 1))
  (define-action "move-column-left" '("<Control><Shift>Left")
              (move-line 'column -1))
  (define-action "move-column-right" '("<Control><Shift>Right")
              (move-line 'column 1))

  (define-action "save" '("<Control>s")
              (lambda ()
                (if *path*
                    (begin (save-sheet sheet *path*)
                           (notify (format #f "Saved ~a" (basename *path*))))
                    (choose-save-path window
                                      (lambda (path)
                                        (set! *path* path)
                                        (save-sheet sheet path)
                                        (retitle)
                                        (notify (format #f "Saved ~a"
                                                        (basename path))))))))

  (define-action "save-as" '("<Control><Shift>s")
              (lambda ()
                (choose-save-path window
                                  (lambda (path)
                                    (set! *path* path)
                                    (save-sheet sheet path)
                                    (retitle)
                                    (notify (format #f "Saved ~a"
                                                    (basename path)))))))

  (define-action "open" '("<Control>o")
              (lambda ()
                (choose-open-path window
                                  (lambda (path)
                                    (set! *path* path)
                                    (load-sheet! sheet path)
                                    (grid-refresh! grid)
                                    (retitle)
                                    (show-selection (grid-active grid))
                                    (notify (format #f "Opened ~a"
                                                    (basename path)))))))

  (define-action "shortcuts" '("<Control>question")
              (lambda () (show-shortcuts window)))

  (define-action "about" '()
              (lambda () (show-about window)))

  (define-action "quit" '("<Control>q")
              (lambda () (gtk-window-close window))))


;;;
;;; Files
;;;

(define (save-sheet sheet path)
  (call-with-output-file path
    (lambda (port)
      (display ";; A Cellar sheet: an alist of cell name to Guile source.\n" port)
      (write (sheet->alist sheet) port)
      (newline port))))

(define (load-sheet! sheet path)
  (let ((data (call-with-input-file path read)))
    (if (list? data)
        (alist->sheet! sheet data)
        (error "cellar: not a sheet file:" path))))

(define (chosen-file finish dialog result)
  "Run FINISH on an async file-dialog result, returning the chosen path or #f.
Cancelling raises a GError, which is the one error we expect and ignore --
anything the caller does with the path is deliberately left outside this
catch, so real failures are not silently swallowed."
  (let ((file (catch #t
                (lambda () (finish dialog result))
                (lambda args #f))))
    (and file (get-path file))))

(define (choose-save-path window continue)
  (let ((dialog (make <gtk-file-dialog>
                  #:title "Save Sheet"
                  #:initial-name "sheet.cellar")))
    (gtk-file-dialog-save
     dialog window #f
     (lambda (dialog result data)
       (let ((path (chosen-file gtk-file-dialog-save-finish dialog result)))
         (when path (continue path))))
     #f)))

(define (choose-open-path window continue)
  (let ((dialog (make <gtk-file-dialog> #:title "Open Sheet")))
    (gtk-file-dialog-open
     dialog window #f
     (lambda (dialog result data)
       (let ((path (chosen-file gtk-file-dialog-open-finish dialog result)))
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
    ("Ctrl+R" . "Recalculate the sheet")
    ("Ctrl+O / Ctrl+S" . "Open / Save")
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
