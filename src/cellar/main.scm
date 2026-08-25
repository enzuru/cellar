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
  ;; Only these three, by name: a bare srfi-1 import would bring in a `map'
  ;; that shadows the one G-Golf exports, and warn about it every build.
  #:use-module ((srfi srfi-1) #:select (find filter-map lset=))
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (main))

(define %rows 100)
(define %columns 26)
(define %application-id "dev.enzuru.Cellar")

;; What the first sheet of a new workbook is called until it is renamed.
(define %first-sheet "Sheet 1")

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

;; The folder of the workbook being edited, or #f when none is open.  A
;; workbook always has one: every edit is written to it as it is made, so there
;; is no such thing as a sheet that is not on disk.
(define *workbook* #f)

;; True when that folder is one Cellar made to hold a scratch workbook, which
;; changes nothing but what the title bar calls it.
(define *scratch?* #f)

;; The GFileMonitors over the open workbook, or #f.
(define *watcher* #f)

;; The paths those monitors are on.  Kept so that the watch is rebuilt when the
;; workbook grows or loses a folder, and left alone when it does not.
(define *watching* '())


;;;
;;; Tabs
;;;

;; A tab is a sheet of the workbook and everything that shows it: the model,
;; the grid over it, and the AdwTabPage it sits in.  A workbook is a list of
;; these, and every action works on whichever one is showing.
;;
;; A tab is found by the title of its page, never by the page itself.  Sheet
;; names are unique within a workbook -- the store refuses a duplicate -- so
;; the title is a key, and using it means nothing here has to reason about
;; whether two Scheme values wrapping the same GObject are the same value.

(define-record-type <tab>
  (%make-tab name page sheet grid)
  tab?
  (name tab-name set-tab-name!)
  (page tab-page)
  (sheet tab-sheet)
  ;; Filled in after the record is made: the grid's callbacks close over the
  ;; tab, so the tab has to exist before the grid does.
  (grid tab-grid set-tab-grid!))


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
           (tab-view (get-object builder "tab_view"))
           (tab-bar (get-object builder "tab_bar"))
           (line-menu (get-object builder "line_menu"))
           (cell-bar (get-object builder "cell_bar"))
           (main-stack (get-object builder "main_stack"))
           (new-sheet-dialog (get-object builder "new_sheet_dialog"))
           (new-sheet-name (get-object builder "new_sheet_name"))
           (new-sheet-location (get-object builder "new_sheet_location"))
           (new-sheet-location-label
            (get-object builder "new_sheet_location_label"))
           (new-sheet-git (get-object builder "new_sheet_git"))
           (sheet-name-dialog (get-object builder "sheet_name_dialog"))
           (sheet-name-entry (get-object builder "sheet_name_entry"))
           (delete-sheet-dialog (get-object builder "delete_sheet_dialog"))
           ;; The tabs of the open workbook, in no particular order; the tab bar
           ;; is what knows the order, and `ordered-tabs' asks it.
           (tabs '())
           ;; True while tabs are being built or torn down, which is when the
           ;; tab view fires the same signals a person switching tabs would.
           ;; Nothing is written to disk during that.
           (loading? #f)
           ;; Where the New Workbook dialog would put a workbook, until told
           ;; otherwise.
           (location (default-location)))

      (define (notify message)
        (add-toast toast-overlay (make <adw-toast> #:title message)))

      (define (sheet-showing?)
        (equal? "sheet" (get-visible-child-name main-stack)))

      (define (retitle)
        (set-subtitle window-title
                      (cond (*scratch?* "Scratch")
                            (*workbook* (workbook-name *workbook*))
                            (else "No workbook open"))))

      (define (show-start-page)
        (set-visible-child-name main-stack "start")
        ;; The cell bar, the tab bar and the recalculate button all speak about
        ;; a workbook; with none open there is nothing for them to say.
        (set-visible cell-bar #f)
        (set-visible tab-bar #f)
        (set-visible recalculate-button #f)
        (retitle))

      (define (show-sheet-page)
        (set-visible-child-name main-stack "sheet")
        (set-visible cell-bar #t)
        (set-visible tab-bar #t)
        (set-visible recalculate-button #t)
        (retitle)
        (let ((tab (current-tab)))
          (when tab
            (show-selection (grid-active (tab-grid tab)))
            (grid-focus! (tab-grid tab)))))

      (define (show-selection r)
        (set-label reference-label (ref->name r))
        (let ((tab (current-tab)))
          (let ((source (and tab (cell-source (tab-sheet tab) r))))
            (if source
                (begin
                  (set-label source-label (one-line source))
                  (remove-css-class source-label "dim-label"))
                (begin
                  (set-label source-label
                             "empty — double-click a cell to write Guile")
                  (add-css-class source-label "dim-label"))))))

      ;;
      ;; Which tab is which
      ;;

      (define (tab-named name)
        (find (lambda (tab) (string=? (tab-name tab) name)) tabs))

      (define (tab-for-page page)
        (and page (tab-named (adw-tab-page-get-title page))))

      (define (current-tab)
        (tab-for-page (adw-tab-view-get-selected-page tab-view)))

      (define (tab-order)
        "The sheet names in the order the tab bar shows them."
        (map (lambda (position)
               (adw-tab-page-get-title
                (adw-tab-view-get-nth-page tab-view position)))
             (iota (adw-tab-view-get-n-pages tab-view))))

      (define (ordered-tabs)
        (filter-map tab-named (tab-order)))

      (define (tab-directory tab)
        (and *workbook* (workbook-sheet-directory *workbook* (tab-name tab))))

      (define (select-tab! name)
        (let ((tab (tab-named name)))
          (when tab
            (adw-tab-view-set-selected-page tab-view (tab-page tab)))))

      ;;
      ;; Building and tearing down tabs
      ;;

      (define (make-sheet-view)
        "A GtkColumnView for one tab's grid.  Made here rather than in the .ui
file because a workbook has one of these for every sheet in it, and a .ui file
can only describe one."
        (let ((view (make <gtk-column-view>)))
          (set-show-row-separators view #t)
          (set-show-column-separators view #t)
          (set-reorderable view #f)
          (add-css-class view "data-table")
          view))

      (define (add-tab! name)
        "Put a tab for sheet NAME on screen, reading it off the disk.  Returns
the tab."
        (let* ((sheet (make-sheet %rows %columns))
               (view (make-sheet-view))
               (scroller (make <gtk-scrolled-window>)))
          (set-hexpand scroller #t)
          (set-vexpand scroller #t)
          (set-child scroller view)
          (let* ((page (adw-tab-view-append tab-view scroller))
                 (tab (%make-tab name page sheet #f)))
            (adw-tab-page-set-title page name)
            (set-tab-grid!
             tab
             (make-grid view sheet line-menu
                        (lambda (r) (when (eq? tab (current-tab))
                                      (show-selection r)))
                        (lambda (r) (edit-cell tab r))
                        (lambda (what) (on-grid-change tab what))))
            (set! tabs (append tabs (list tab)))
            (load-tab! tab)
            tab)))

      (define (load-tab! tab)
        "Read the sheet a tab stands for off the disk and into it."
        (let ((directory (tab-directory tab)))
          (when (and directory (sheet-directory? directory))
            (catch #t
              (lambda ()
                (let ((widths (load-sheet! (tab-sheet tab) directory)))
                  (grid-sync-size! (tab-grid tab))
                  (grid-set-column-widths! (tab-grid tab) widths)
                  (grid-set-active! (tab-grid tab) (make-ref 0 0))
                  (grid-refresh! (tab-grid tab))))
              (lambda (key . args)
                (report-failure (format #f "read ~a" (tab-name tab)) key args))))))

      (define (close-all-tabs!)
        "Take every tab off screen.  This says nothing about the disk -- it is
what opening another workbook does, not what deleting a sheet does."
        (let ((closing tabs))
          (set! tabs '())
          (for-each (lambda (tab)
                      (adw-tab-view-close-page tab-view (tab-page tab)))
                    closing)))

      (define (forget-tab! tab)
        (set! tabs (filter (lambda (other) (not (eq? other tab))) tabs)))

      ;;
      ;; Saving.  There is no command for it: a cell is written the moment it
      ;; is changed, so the folder on disk is what the workbook is rather than
      ;; a copy of it taken when you remembered to ask.
      ;;

      (define (persist-cell! tab r)
        "Write one cell to its own file."
        (let ((directory (tab-directory tab)))
          (when directory
            (catch #t
              (lambda ()
                (save-cell! directory (ref->name r)
                            (cell-source (tab-sheet tab) r)))
              (lambda (key . args)
                (report-failure (format #f "save ~a" (ref->name r)) key args))))))

      (define (persist-layout! tab)
        "Write the size of one sheet and its column widths."
        (let ((directory (tab-directory tab)))
          (when directory
            (catch #t
              (lambda ()
                (write-sheet-metadata! directory
                                       (sheet-rows (tab-sheet tab))
                                       (sheet-columns (tab-sheet tab))
                                       (grid-column-widths (tab-grid tab))))
              (lambda (key . args) (report-failure "save the sheet" key args))))))

      (define (persist-cells! tab)
        "Write every cell of one sheet.  Moving a row or inserting a column
renames the files of every cell it shifted, so the cheapest correct answer for
those is to write the lot -- it is a few dozen small files, and it deletes the
ones left behind."
        (let ((directory (tab-directory tab)))
          (when directory
            (catch #t
              (lambda ()
                (save-sheet! (tab-sheet tab) directory
                             (grid-column-widths (tab-grid tab))))
              (lambda (key . args) (report-failure "save the sheet" key args))))))

      (define (persist-order!)
        "Write the order the tabs are in."
        (when (and *workbook* (not loading?))
          (catch #t
            (lambda () (set-workbook-order! *workbook* (tab-order)))
            (lambda (key . args)
              (report-failure "save the order of the sheets" key args)))))

      (define (persist-active!)
        "Write which tab is showing, so that reopening the workbook comes back
to the sheet you left."
        (let ((tab (and *workbook* (not loading?) (current-tab))))
          (when tab
            (catch #t
              (lambda () (set-workbook-active! *workbook* (tab-name tab)))
              (lambda (key . args) (report-failure "save the workbook" key args))))))

      (define (persist-fresh-layouts!)
        "Write the size of every sheet of a workbook Cellar has just made.  A
sheet folder is created empty -- nought by nought -- while the sheet in front
of you is 100 by 26, and this is what makes the file say what the window says.
Only ever called on a folder we made a moment ago, so it is not a write into
somebody's workbook for merely having opened it."
        (for-each persist-layout! tabs))

      (define (on-grid-change tab what)
        (if (eq? what 'layout) (persist-layout! tab) (persist-cells! tab)))

      (define (apply-edit tab r)
        (lambda (text)
          (set-cell-source! (tab-sheet tab) r text)
          (persist-cell! tab r)
          (grid-refresh! (tab-grid tab))
          (when (eq? tab (current-tab)) (show-selection r))))

      (define (edit-internally tab r)
        (open-cell-editor ui-directory window (tab-sheet tab) r
                          (apply-edit tab r)))

      (define (edit-cell tab r)
        "Open the cell wherever the preferences say. If the external editor
cannot be started we say so and fall back, rather than leaving a cell that
cannot be edited at all."
        (let ((command (effective-editor-command))
              (directory (tab-directory tab)))
          (if (and command directory)
              (if (open-external-editor command directory r notify)
                  (notify (format #f "Editing ~a in ~a"
                                  (ref->name r) (external-editor-name command)))
                  (edit-internally tab r))
              (edit-internally tab r))))

      ;;
      ;; Catching up with the disk
      ;;

      (define (rewatch!)
        "Watch the open workbook, and only it.  Which sheets there are is part
of what changes, so this is redone whenever they do."
        (unwatch! *watcher*)
        (set! *watching* (if *workbook* (workbook-watch-paths *workbook*) '()))
        (set! *watcher*
              (and *workbook* (watch-paths! *watching* reload-from-disk))))

      (define (rewatch-if-changed!)
        "Rebuild the watch when the workbook has gained or lost a folder since
it was last set up.

A sheet arriving from outside is a folder that appears and is filled in a
moment later.  The folder appearing is what wakes us; by the time we look there
may be nothing in it yet, and if we did not take a watch out on it here we
would never hear about it being filled.  A cell being written changes no path
at all, so the ordinary case costs one comparison and no monitors."
        (when (and *workbook*
                   (not (equal? *watching* (workbook-watch-paths *workbook*))))
          (rewatch!)))

      (define (reload-from-disk)
        "Something under the workbook folder changed; take the folder as the
truth.

Cellar's own writes come through here too, and have to be harmless when they
do -- which they are, because by the time the file lands the model already says
what the file says, and the comparisons below find nothing to do.  That is the
whole reason this compares rather than tries to remember which files were ours."
        (when (and *workbook* (workbook-directory? *workbook*))
          (catch #t
            (lambda ()
              (rewatch-if-changed!)
              (let ((names (workbook-sheet-names *workbook*)))
                (if (lset= string=? names (map tab-name tabs))
                    ;; The same sheets as before: each one catches up on its own.
                    (let ((changed (filter reload-tab! tabs)))
                      (when (pair? changed)
                        (notify "Reloaded — the workbook changed on disk")))
                    ;; A sheet arrived or left -- somebody's commit, most
                    ;; likely.  Rebuilding the tabs is the honest answer, and
                    ;; cheap enough that it is not worth a cleverer one.
                    (begin
                      (reopen-workbook!)
                      (notify "Reloaded — the sheets changed on disk")))))
            (lambda (key . args)
              (report-failure "read the workbook" key args)))))

      (define (reload-tab! tab)
        "Bring one tab back in line with its folder.  Returns #t if anything
had in fact changed."
        (let ((directory (tab-directory tab))
              (sheet (tab-sheet tab))
              (grid (tab-grid tab)))
          (and directory
               (sheet-directory? directory)
               (let ((on-disk (read-sheet-cells directory)))
                 (and (not (equal? on-disk (cells-by-name sheet)))
                      (let ((metadata (read-sheet-metadata directory))
                            (active (grid-active grid)))
                        (alist->sheet! sheet on-disk)
                        (grow-sheet! sheet
                                     (or (assq-ref metadata 'rows) 0)
                                     (or (assq-ref metadata 'columns) 0))
                        (grid-sync-size! grid)
                        ;; Keep the cursor where the user left it, unless the
                        ;; sheet shrank out from under it.
                        (grid-set-active! grid
                                          (if (valid-ref? sheet active)
                                              active
                                              (make-ref 0 0)))
                        (grid-refresh! grid)
                        #t))))))

      (define (reopen-workbook!)
        "Throw the tabs away and build them again from the folder, coming back
to the sheet that was showing if it is still there."
        (let ((showing (and (current-tab) (tab-name (current-tab)))))
          (build-tabs! (or showing (workbook-active-sheet *workbook*)))
          (rewatch!)))

      (define (build-tabs! showing)
        "Make a tab for every sheet in the workbook and select SHOWING."
        (set! loading? #t)
        (close-all-tabs!)
        (let ((names (workbook-sheet-names *workbook*)))
          (for-each add-tab! names)
          (select-tab! (if (and showing (member showing names))
                           showing
                           (or (workbook-active-sheet *workbook*)
                               (and (pair? names) (car names))))))
        (set! loading? #f)
        (let ((tab (current-tab)))
          (when tab
            (show-selection (grid-active (tab-grid tab)))
            (grid-focus! (tab-grid tab)))))

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
      ;; Opening and making workbooks
      ;;

      ;; Which of the two questions the New Workbook dialog is currently asking.
      (define *copying?* #f)

      (define (open-workbook path)
        "Open the workbook PATH names, by its folder, its index, or the
primary file of any sheet inside it."
        (let ((directory (workbook-directory path)))
          (if (not (workbook-directory? directory))
              (begin
                (notify (format #f "~a is not a Cellar workbook"
                                (basename directory)))
                #f)
              (catch #t
                (lambda ()
                  (set! *workbook* directory)
                  (set! *scratch?* #f)
                  (build-tabs! #f)
                  (rewatch!)
                  (show-sheet-page)
                  #t)
                (lambda (key . args)
                  (report-failure (format #f "open ~a" (basename directory))
                                  key args)
                  #f)))))

      (define (scratch-workbook)
        "A workbook to think in.  It still lives in a folder -- everything does
now -- but one Cellar picks, out of the way under the data directory, so that
starting one asks nothing.  Copy To puts it somewhere you chose."
        (catch #t
          (lambda ()
            (let ((directory (scratch-location)))
              (create-workbook! directory %first-sheet #f)
              (when (open-workbook directory)
                (set! *scratch?* #t)
                (persist-fresh-layouts!)
                (retitle))))
          (lambda (key . args)
            (report-failure "make a scratch workbook" key args))))

      (define (copy-to directory git?)
        "Write every sheet of the workbook to a folder of its own and carry on
editing it there.  The workbook you were in is left exactly as it was."
        (catch #t
          (lambda ()
            (let* ((sheets (ordered-tabs))
                   (first (tab-name (car sheets)))
                   (made (create-workbook! directory first git?)))
              (for-each (lambda (tab)
                          (unless (string=? (tab-name tab) first)
                            (add-workbook-sheet! directory (tab-name tab))))
                        sheets)
              (for-each (lambda (tab)
                          (save-sheet! (tab-sheet tab)
                                       (workbook-sheet-directory
                                        directory (tab-name tab))
                                       (grid-column-widths (tab-grid tab))))
                        sheets)
              (write-workbook-index! directory (map tab-name sheets)
                                     (and (current-tab)
                                          (tab-name (current-tab))))
              (set! *workbook* directory)
              (set! *scratch?* #f)
              (rewatch!)
              (retitle)
              (when (eq? made 'created-without-git)
                (notify "The folder was made, but git could not be run"))
              (notify (format #f "Now editing ~a" (workbook-name directory)))))
          (lambda (key . args)
            (report-failure (format #f "copy to ~a" (basename directory))
                            key args))))

      (define (ask-for-new-workbook suggestion copying?)
        "The same dialog for New Workbook and for Copy To: both are a name and
a place to put it, and COPYING? decides which of the two the answer means."
        (set! *copying?* copying?)
        (set-heading new-sheet-dialog
                     (if copying? "Copy Workbook To" "New Workbook"))
        (set-body new-sheet-dialog
                  (if copying?
                      "The workbook is written to a new folder, and that is the one you carry on editing. The folder you were in is left as it stands."
                      "A workbook is a folder, and a Git repository worth making one of: a folder for each sheet in it, and one small file for every cell."))
        (adw-alert-dialog-set-response-label new-sheet-dialog "create"
                                             (if copying? "Copy" "Create"))
        (gtk-editable-set-text new-sheet-name suggestion)
        (set-label new-sheet-location-label location)
        (present new-sheet-dialog window))

      (define (create-new-workbook)
        (let* ((typed (string-trim-both (gtk-editable-get-text new-sheet-name)))
               (name (if (string-null? typed) "workbook" typed))
               (directory (string-append location "/"
                                         (workbook-folder-name name)))
               (git? (get-active new-sheet-git)))
          (if *copying?*
              (copy-to directory git?)
              (catch #t
                (lambda ()
                  (let ((made (create-workbook! directory %first-sheet git?)))
                    (when (open-workbook directory)
                      (persist-fresh-layouts!)
                      (notify (format #f "Created ~a"
                                      (workbook-name directory))))
                    (when (eq? made 'created-without-git)
                      (notify "The folder was made, but git could not be run"))))
                (lambda (key . args)
                  (report-failure (format #f "create ~a" (basename directory))
                                  key args))))))


      ;;
      ;; Adding, renaming and deleting sheets
      ;;

      ;; The tab whose name is being asked for, or #f when the answer is meant
      ;; to add a new sheet rather than rename an old one.
      (define *renaming* #f)

      (define (ask-for-sheet-name tab)
        "The same dialog for Add Sheet and Rename Sheet: both are a name, and
TAB -- the sheet being renamed, or #f -- decides what the answer means."
        (set! *renaming* tab)
        (set-heading sheet-name-dialog (if tab "Rename Sheet" "Add Sheet"))
        (set-body sheet-name-dialog
                  (if tab
                      "The sheet's folder is renamed along with it, so a cell keeps the history it already had."
                      "A sheet is a folder of its own inside the workbook, so its name is a folder's name."))
        (adw-alert-dialog-set-response-label sheet-name-dialog "name"
                                             (if tab "Rename" "Add"))
        (gtk-editable-set-text sheet-name-entry
                               (if tab
                                   (tab-name tab)
                                   (unique-sheet-name
                                    *workbook*
                                    (format #f "Sheet ~a" (+ 1 (length tabs))))))
        (present sheet-name-dialog window))

      (define (add-sheet name)
        (catch #t
          (lambda ()
            ;; A workbook written before there were tabs is moved into sheets/
            ;; by this, which changes where its one sheet is written but
            ;; nothing about what it says.  The tabs are rebuilt rather than
            ;; added to, so that the sheet that moved is reopened from where it
            ;; now lives.
            (let* ((migrated? (workbook-format-1? *workbook*))
                   (added (add-workbook-sheet! *workbook* name)))
              (if migrated?
                  (begin
                    (build-tabs! added)
                    (let ((tab (tab-named added)))
                      (when tab (persist-layout! tab))))
                  (let ((tab (add-tab! added)))
                    (select-tab! added)
                    (persist-layout! tab)))
              (rewatch!)
              (notify (format #f "Added ~a" added))))
          (lambda (key . args) (report-failure "add the sheet" key args))))

      (define (rename-sheet tab name)
        (catch #t
          (lambda ()
            (let ((renamed (rename-workbook-sheet! *workbook*
                                                   (tab-name tab) name)))
              (set-tab-name! tab renamed)
              (adw-tab-page-set-title (tab-page tab) renamed)
              (rewatch!)
              (retitle)))
          (lambda (key . args) (report-failure "rename the sheet" key args))))

      ;; The tab that is being asked about, and the AdwTabPage waiting to hear
      ;; whether it may close.
      (define *pending-delete* #f)

      (define (ask-to-delete tab page)
        (set! *pending-delete* (cons tab page))
        (set-heading delete-sheet-dialog
                     (format #f "Delete ~a?" (tab-name tab)))
        (set-body delete-sheet-dialog
                  "The sheet's folder and every cell file in it are deleted from the workbook. Cellar cannot undo that — though if the workbook is a Git repository, Git can.")
        (present delete-sheet-dialog window))

      (define (delete-sheet! tab)
        "Take the sheet off the disk.  Returns #t when it went, which is what
tells the tab it may close."
        (catch #t
          (lambda ()
            (remove-workbook-sheet! *workbook* (tab-name tab))
            (forget-tab! tab)
            (rewatch!)
            (notify (format #f "Deleted ~a" (tab-name tab)))
            #t)
          (lambda (key . args)
            (report-failure "delete the sheet" key args)
            #f)))

      (define (on-close-page page)
        "A tab's close button, or Delete Sheet.  A tab is a sheet of the
workbook rather than a view of one, so closing it is deleting it -- which is
worth being asked about, and worth refusing when it would leave the workbook
with nothing in it."
        (let ((tab (tab-for-page page)))
          (cond
           ;; Tabs being torn down to open another workbook.  Nothing is being
           ;; deleted, so let the default handler take the page away.
           (loading? #f)
           ((not tab)
            (adw-tab-view-close-page-finish tab-view page #t)
            #t)
           ((null? (cdr tabs))
            (notify "A workbook has to keep at least one sheet")
            (adw-tab-view-close-page-finish tab-view page #f)
            #t)
           (else
            (ask-to-delete tab page)
            #t))))

      (define (on-tab-selected)
        (let ((tab (current-tab)))
          (when tab
            (show-selection (grid-active (tab-grid tab)))
            (persist-active!))))

      (define (step-sheet delta)
        "Move to the tab DELTA along, without wrapping."
        (lambda ()
          (let ((pages (adw-tab-view-get-n-pages tab-view))
                (page (adw-tab-view-get-selected-page tab-view)))
            (when (and page (> pages 0))
              (let ((next (+ delta (adw-tab-view-get-page-position
                                    tab-view page))))
                (when (and (>= next 0) (< next pages))
                  (adw-tab-view-set-selected-page
                   tab-view (adw-tab-view-get-nth-page tab-view next))))))))


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

      ;; An action that means nothing with no workbook on screen, and does
      ;; nothing there: the shortcuts are live whichever page is showing.
      ;; PROCEDURE is given the tab that is showing.
      (define (on-sheet procedure)
        (lambda ()
          (let ((tab (and (sheet-showing?) (current-tab))))
            (when tab (procedure tab)))))

      (define (install-actions)
        (define-action "new" '("<Control>n")
                    (lambda () (ask-for-new-workbook "workbook" #f)))

        (define-action "new-scratch" '("<Control><Shift>n")
                    (lambda () (scratch-workbook)))

        (define-action "open" '("<Control>o")
                    (lambda ()
                      (choose-folder window (lambda (path) (open-workbook path)))))

        ;; There is nothing to save: the workbook on disk is already this one.
        ;; Ctrl+S is too deep a reflex to leave doing nothing silently, so it
        ;; says so instead.
        (define-action "save" '("<Control>s")
                    (on-sheet
                     (lambda (tab)
                       (notify "Cellar saves each cell as you edit it"))))

        (define-action "copy-to" '("<Control><Shift>s")
                    (on-sheet
                     (lambda (tab)
                       (ask-for-new-workbook
                        (if (and *workbook* (not *scratch?*))
                            (workbook-name *workbook*)
                            "workbook")
                        #t))))

        ;; Sheets.  A tab is a folder in the workbook, so adding, renaming and
        ;; deleting one is a change to the folder as much as to the screen.
        (define-action "add-sheet" '("<Control>t")
                    (on-sheet (lambda (tab) (ask-for-sheet-name #f))))

        (define-action "rename-sheet" '("<Control><Shift>r")
                    (on-sheet (lambda (tab) (ask-for-sheet-name tab))))

        (define-action "delete-sheet" '("<Control>w")
                    (on-sheet
                     (lambda (tab)
                       (adw-tab-view-close-page tab-view (tab-page tab)))))

        (define-action "next-sheet" '("<Control>Page_Down")
                    (on-sheet (lambda (tab) ((step-sheet 1)))))

        (define-action "previous-sheet" '("<Control>Page_Up")
                    (on-sheet (lambda (tab) ((step-sheet -1)))))

        (define-action "recalculate" '("<Control>r")
                    (on-sheet
                     (lambda (tab)
                       (invalidate-sheet! (tab-sheet tab))
                       (grid-refresh! (tab-grid tab))
                       (notify "Recalculated"))))

        (define-action "clear-cell" '("Delete")
                    (on-sheet
                     (lambda (tab)
                       (let ((r (grid-active (tab-grid tab))))
                         (set-cell-source! (tab-sheet tab) r "")
                         (persist-cell! tab r)
                         (grid-refresh! (tab-grid tab))
                         (show-selection r)))))

        (define-action "edit-cell" '("<Control>e")
                    (on-sheet
                     (lambda (tab) (edit-cell tab (grid-active (tab-grid tab))))))

        ;; Reordering. The model rewrites references as it moves cells, so a
        ;; sheet means the same thing after a move as it did before; all that
        ;; changes is where you read it.
        (define (move-line axis delta)
          (on-sheet
           (lambda (tab)
             (unless (grid-move-line! (tab-grid tab) axis delta)
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
           (lambda (tab)
             (when (grid-insert-line! (tab-grid tab) axis where)
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

      (connect edit-button 'clicked
               (lambda (button)
                 (let ((tab (current-tab)))
                   (when tab (edit-cell tab (grid-active (tab-grid tab)))))))
      (connect recalculate-button 'clicked
               (lambda (button)
                 (let ((tab (current-tab)))
                   (when tab
                     (invalidate-sheet! (tab-sheet tab))
                     (grid-refresh! (tab-grid tab))
                     (notify "Recalculated")))))
      (connect new-sheet-location 'clicked
               (lambda (button)
                 (choose-folder window
                                (lambda (path)
                                  (set! location path)
                                  (set-label new-sheet-location-label path)))))
      (connect new-sheet-dialog 'response
               (lambda (dialog response)
                 (when (equal? response "create")
                   (create-new-workbook))))
      (connect sheet-name-dialog 'response
               (lambda (dialog response)
                 (when (equal? response "name")
                   (let ((typed (string-trim-both
                                 (gtk-editable-get-text sheet-name-entry)))
                         (tab *renaming*))
                     (set! *renaming* #f)
                     (if tab (rename-sheet tab typed) (add-sheet typed))))))
      (connect delete-sheet-dialog 'response
               (lambda (dialog response)
                 (let ((pending *pending-delete*))
                   (set! *pending-delete* #f)
                   (when pending
                     (adw-tab-view-close-page-finish
                      tab-view (cdr pending)
                      (and (equal? response "delete")
                           (delete-sheet! (car pending))))))))

      (connect tab-view 'close-page
               (lambda (view page) (on-close-page page)))
      (connect tab-view 'page-reordered
               (lambda (view page position) (persist-order!)))
      (connect tab-view 'notify::selected-page
               (lambda (view pspec) (on-tab-selected)))

      (install-actions)
      (show-start-page)

      (add-window application window)
      (present window)

      ;; A workbook named on the command line opens straight away; without one
      ;; the start page asks what to open, since a sheet has to live somewhere.
      (when file
        (unless (open-workbook file)
          (show-start-page))))))

(define (default-location)
  (or (getenv "HOME") (getcwd)))

(define (scratch-location)
  "A folder of its own for a scratch workbook, under the XDG data directory and
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

(define (workbook-folder-name name)
  "A workbook's folder is named like a file would be: budget -> budget.cellar."
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

A workbook is a folder, so this is the one chooser the application needs:
opening one picks the folder, and making one picks the folder to make it in."
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
    ("Ctrl+T" . "Add a sheet to this workbook")
    ("Ctrl+Shift+R" . "Rename the sheet showing")
    ("Ctrl+W" . "Delete the sheet showing")
    ("Ctrl+Page Up / Page Down" . "Move to the sheet before or after this one")
    ("Drag a tab" . "Reorder the sheets")
    ("Ctrl+N / Ctrl+Shift+N" . "New workbook / New scratch workbook")
    ("Ctrl+O" . "Open a workbook folder")
    ("Ctrl+Shift+S" . "Copy this workbook to another folder")
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
