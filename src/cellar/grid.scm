;;; Cellar -- the spreadsheet grid.
;;;
;;; GTK4 has no spreadsheet widget, so this builds one out of GtkColumnView:
;;; one GtkColumnViewColumn per spreadsheet column, each with a
;;; GtkSignalListItemFactory whose callbacks close over the column index.
;;;
;;; The trick that keeps this simple is that the list model carries no data at
;;; all. It is a GtkStringList of row numbers, purely to give the view the right
;;; number of rows; the cell contents come from the Scheme-side sheet, looked up
;;; with gtk_column_view_cell_get_position() at bind time. That means no custom
;;; GObject item class, and no C.
;;;
;;; Only the visible rows are ever realised -- GtkColumnView recycles cell
;;; widgets as you scroll -- so a 100x26 sheet costs a few hundred widgets
;;; rather than 2600.

(define-module (cellar grid)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar model)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (make-grid
            grid?
            grid-sheet
            grid-active
            grid-set-active!
            grid-refresh!
            grid-move-active!
            grid-move-line!
            grid-insert-line!
            grid-sync-size!
            grid-column-widths
            grid-set-column-widths!
            grid-focus!))

;; GTK_INVALID_LIST_POSITION -- what get-position returns for an unbound cell.
(define %invalid-position #xffffffff)

(define %column-width 104)
(define %gutter-width 60)

(define-record-type <grid>
  (%make-grid view sheet cells active on-select on-activate on-change titles
              drag row-list row-count columns menu)
  grid?
  (view grid-view)
  (sheet grid-sheet)
  ;; Every (cell label column) triple handed to us by a factory `setup'.
  ;; Cell widgets are recycled, so this list stays proportional to the visible
  ;; area, not to the size of the sheet.
  (cells grid-cells set-grid-cells!)
  (active grid-active %set-grid-active!)
  (on-select grid-on-select)
  (on-activate grid-on-activate)
  ;; Called with a symbol -- 'cells or 'layout -- whenever the grid itself has
  ;; changed the sheet: a row moved, a column inserted, a header dragged
  ;; wider.  What the caller does with it is write the change to disk.
  (on-change grid-on-change)
  ;; The column header widgets, each paired with its column's index box.  A
  ;; pairing rather than an order: GtkColumnView adds an inserted column's
  ;; header at the end of the header row whatever the column's position, so
  ;; the header row is not in column order.  See `install-header-drag!'.
  (titles grid-titles set-grid-titles!)
  ;; The drag in progress, or #f.
  (drag grid-drag set-grid-drag!)
  ;; The GtkStringList standing in for the rows, and how many entries it has:
  ;; the view grows a row by growing this.  GtkStringList can be counted
  ;; through GListModel, but not from here -- G-Golf does not marshal it --
  ;; so the count is kept alongside.
  (row-list grid-row-list)
  (row-count grid-row-count set-grid-row-count!)
  ;; The sheet's columns as (index . view-column) pairs, in column order.
  (columns grid-columns set-grid-columns!)
  ;; The GtkPopoverMenu a right-click on the gutter or a header opens, or #f.
  (menu grid-menu set-grid-menu!))

(define-record-type <drag>
  (make-drag axis from widget start-x start-y target)
  drag?
  ;; 'row or 'column: which of the two the user has hold of.
  (axis drag-axis)
  (from drag-from)
  ;; The widget the gesture is on, and where in it the drag started, so that a
  ;; drag-update offset can be turned back into a point.
  (widget drag-widget)
  (start-x drag-start-x)
  (start-y drag-start-y)
  (target drag-target set-drag-target!))


;; A column's index is not fixed for the life of the column: inserting a column
;; shifts every column to its right one place along.  The factory callbacks and
;; the header gesture are installed once and there is nowhere to tell them, so
;; they close over a box that an insert renumbers rather than over the number.
(define (make-column-index n) (list n))
(define (column-index box) (car box))
(define (set-column-index! box n) (set-car! box n))

(define (make-grid view sheet line-menu on-select on-activate on-change)
  "Turn VIEW, a GtkColumnView, into a grid over SHEET.
LINE-MENU is the GMenuModel offered when a row number or a column header is
right-clicked, or #f for no menu at all.
ON-SELECT is called with a reference whenever the active cell changes.
ON-ACTIVATE is called with a reference when a cell is double-clicked or
activated from the keyboard.
ON-CHANGE is called with 'cells when the grid has moved or inserted something,
and with 'layout when a column has been resized."
  (let* ((rows (row-model sheet))
         (grid (%make-grid view sheet '() (make-ref 0 0) on-select on-activate
                           on-change '() #f rows (sheet-rows sheet) '() #f)))
    (set-model view (gtk-no-selection-new rows))
    (install-line-menu! grid line-menu)
    ;; A narrow leading column of row numbers, standing in for the row headers
    ;; GtkColumnView does not have.
    (append-column view
                   (make-column grid (make-column-index -1) "" %gutter-width))
    (for-each (lambda (column) (add-column! grid column))
              (iota (sheet-columns sheet)))
    (install-key-handling grid)
    ;; The header widgets may not exist yet; if they do not, realise will do it.
    (install-header-drag! grid)
    (connect view 'realize (lambda (view) (install-header-drag! grid)))
    grid))

(define (row-model sheet)
  "A GtkStringList with one entry per row. The strings are never read; the
model exists only to give the view its row count."
  (let ((model (make <gtk-string-list>)))
    (for-each (lambda (row) (append model (number->string row)))
              (iota (sheet-rows sheet)))
    model))

(define (make-column grid index title width)
  (let ((factory (make <gtk-signal-list-item-factory>)))
    (connect factory 'setup
             (lambda (factory cell) (setup-cell grid index cell)))
    (connect factory 'bind
             (lambda (factory cell) (bind-cell grid index cell)))
    (let ((view-column (gtk-column-view-column-new title factory)))
      (set-fixed-width view-column width)
      (set-resizable view-column (>= (column-index index) 0))
      ;; Connected after the width above is set, so building a column is not
      ;; itself a change.  Dragging a header's edge is GTK's own gesture, which
      ;; Cellar deliberately stays out of; this property is where it shows up.
      (connect view-column 'notify::fixed-width
               (lambda (column pspec) ((grid-on-change grid) 'layout)))
      view-column)))


;;;
;;; Cell widgets
;;;

(define (setup-cell grid index cell)
  ;; Whether this is the gutter or a cell of the sheet is settled here and for
  ;; good; only the column's number can change under it.  The label fills its
  ;; cell -- its padding is in the stylesheet rather than margins here, so that
  ;; a cell's background colour reaches the cell's edges.
  (let* ((column (column-index index))
         (label (make <gtk-label>
                  #:hexpand #t
                  #:xalign (if (< column 0) 0.5 0.0)
                  #:ellipsize 'end
                  #:single-line-mode #t)))
    (add-css-class label (if (< column 0) "cellar-gutter" "cellar-cell"))
    (set-child cell label)
    (if (< column 0)
        ;; The gutter is the row's handle: it is the one part of a row that
        ;; holds no data, so dragging it can only mean "move this row".
        (begin
          (gtk-widget-set-cursor-from-name label "grab")
          (install-row-drag! grid cell label)
          (install-line-click! grid label 'row
                               (lambda ()
                                 (let ((position (get-position cell)))
                                   (and (live-position? grid position)
                                        position)))))
        (let ((gesture (gtk-gesture-click-new)))
          (connect gesture 'pressed
                   (lambda (gesture n-press x y)
                     (cell-pressed grid index cell n-press)))
          (add-controller label gesture)))
    (set-grid-cells! grid (cons (list cell label index) (grid-cells grid)))))

(define (bind-cell grid index cell)
  (let ((position (get-position cell)))
    (when (live-position? grid position)
      (paint-cell grid (get-child cell) (column-index index) position))))

(define (live-position? grid position)
  (and (< position %invalid-position)
       (< position (sheet-rows (grid-sheet grid)))))

(define (paint-cell grid label column row)
  (if (< column 0)
      (set-label label (number->string (+ row 1)))
      (let* ((sheet (grid-sheet grid))
             (r (make-ref row column))
             (value (cell-value sheet r))
             (source (cell-source sheet r)))
        (set-label label (cell-display sheet r))
        ;; Numbers right-align, everything else left-aligns, as in any sheet.
        (set-xalign label (if (and value (number? value)) 1.0 0.0))
        (if (cell-error? value)
            (begin
              (add-css-class label "cellar-error")
              (set-tooltip-text label (cell-error-message value)))
            (begin
              (remove-css-class label "cellar-error")
              (set-tooltip-text label source)))
        (if (equal? r (grid-active grid))
            (add-css-class label "cellar-active")
            (remove-css-class label "cellar-active"))
        (paint-style label (cell-style sheet r)))))


;;;
;;; Cell colours
;;;

;; A cell can ask for colours of its own -- (styled (* B2 C2) #:background
;; "#fff3b0"). GTK has no per-widget colour to set and no supported way to give
;; one widget a stylesheet of its own, so each distinct (colour . background)
;; pair earns a CSS class in a provider belonging to this module. The palette
;; is whatever the sheet actually asks for, which is a handful of entries even
;; for a large sheet, and the classes outlive the cell widgets that GtkColumnView
;; recycles underneath them.

(define %style-prefix "cellar-style-")
(define %palette (make-hash-table))
(define %palette-provider #f)

(define (paint-style label style)
  "Dress LABEL in STYLE, a (colour . background) pair, or strip it when #f."
  (for-each (lambda (class)
              (when (string-prefix? %style-prefix class)
                (remove-css-class label class)))
            (get-css-classes label))
  (when style
    (add-css-class label (style-class style))))

(define (style-class style)
  "The CSS class that paints STYLE, defining it on first sight."
  (or (hash-ref %palette style)
      (let ((class (string-append %style-prefix
                                  (number->string (hash-count (const #t) %palette)))))
        (hash-set! %palette style class)
        (install-palette!)
        class)))

(define (install-palette!)
  (unless %palette-provider
    (set! %palette-provider (make <gtk-css-provider>))
    ;; Above the application's own stylesheet, so that a cell asking for a
    ;; colour outranks the grid's idea of what a cell looks like.
    (gtk-style-context-add-provider-for-display
     (gdk-display-get-default) %palette-provider 700))
  (load-from-string %palette-provider (palette-css)))

(define (palette-css)
  (string-join
   (hash-map->list
    (lambda (style class)
      (format #f ".~a { ~a~a}"
              class
              (if (car style) (format #f "color: ~a; " (car style)) "")
              (if (cdr style) (format #f "background-color: ~a; " (cdr style)) "")))
    %palette)
   "\n"))

(define (grid-refresh! grid)
  "Repaint every realised cell. Called after the sheet changes."
  ;; A column added since the last refresh has a header widget by now, which it
  ;; had not when it was inserted -- GtkColumnView builds those on the next
  ;; layout pass -- so this is where it is given its drag gesture.
  (install-header-drag! grid)
  (for-each (lambda (entry)
              (let ((cell (first entry))
                    (label (second entry))
                    (column (column-index (third entry))))
                (let ((position (get-position cell)))
                  (when (live-position? grid position)
                    (paint-cell grid label column position)))))
            (grid-cells grid)))


;;;
;;; Selection
;;;

(define (cell-pressed grid index cell n-press)
  (let ((position (get-position cell)))
    (when (live-position? grid position)
      (let ((r (make-ref position (column-index index))))
        (grid-set-active! grid r)
        ;; The whole point of the app: a second click opens the editor.
        (when (>= n-press 2)
          ((grid-on-activate grid) r))))))

(define (grid-set-active! grid r)
  (when (valid-ref? (grid-sheet grid) r)
    (%set-grid-active! grid r)
    (grid-refresh! grid)
    ((grid-on-select grid) r)))

(define (grid-move-active! grid row-delta column-delta)
  (let* ((sheet (grid-sheet grid))
         (current (grid-active grid))
         (row (clamp (+ (ref-row current) row-delta) 0 (- (sheet-rows sheet) 1)))
         (column (clamp (+ (ref-column current) column-delta)
                        0 (- (sheet-columns sheet) 1))))
    (grid-set-active! grid (make-ref row column))
    (scroll-to-row grid row)))

(define (clamp n low high) (max low (min high n)))

;; Moving a whole row or column is a model operation -- the sheet rewrites the
;; references inside every cell so they follow the cells they name -- so all the
;; grid does is ask for it, and then follow the cells on screen.
(define (grid-move-line! grid axis delta)
  "Move the active cell's row (AXIS 'row) or column (AXIS 'column) DELTA places.
Returns #t when the sheet changed."
  (let* ((active (grid-active grid))
         (from (if (eq? axis 'row) (ref-row active) (ref-column active))))
    (and (apply-move! grid axis from (+ from delta))
         (begin (scroll-to-row grid (ref-row (grid-active grid))) #t))))

(define (apply-move! grid axis from to)
  "Move a row or column in the sheet, keeping the active cell on its cell.
Returns #t when the sheet changed."
  (let ((sheet (grid-sheet grid)))
    (and (if (eq? axis 'row)
             (move-row! sheet from to)
             (move-column! sheet from to))
         (begin
           (%set-grid-active! grid
                              (ref-after-move (grid-active grid) axis from to))
           (grid-refresh! grid)
           ((grid-on-select grid) (grid-active grid))
           ((grid-on-change grid) 'cells)
           #t))))


;;;
;;; Adding a row or a column
;;;

;; As with a move, the sheet does the work that matters -- it shifts the cells
;; and rewrites the references inside them -- and the view is brought up to
;; match afterwards.  A row is one more entry in the list model.  A column is
;; one more GtkColumnViewColumn, inserted in place, after which every column
;; from there rightwards answers to a new index and a new letter.

(define (grid-insert-line! grid axis where)
  "Open an empty row (AXIS 'row) or column (AXIS 'column) at the active cell.
WHERE is 'before or 'after.  The active cell stays on the cell it was on, so an
insert above it carries it down.  Returns #t when the sheet changed."
  (let* ((sheet (grid-sheet grid))
         (active (grid-active grid))
         (at (+ (if (eq? axis 'row) (ref-row active) (ref-column active))
                (if (eq? where 'before) 0 1))))
    (and (if (eq? axis 'row)
             (insert-row! sheet at)
             (insert-column! sheet at))
         (begin
           (if (eq? axis 'row)
               (add-row! grid)
               (add-column! grid at))
           (%set-grid-active! grid (ref-after-insert active axis at))
           (grid-refresh! grid)
           ((grid-on-select grid) (grid-active grid))
           ((grid-on-change grid) 'cells)
           (scroll-to-row grid (ref-row (grid-active grid)))
           #t))))

(define (add-row! grid)
  "One more row in the view.  The strings in the model are never read, so where
the entry goes does not matter -- only how many there are."
  (gtk-string-list-append (grid-row-list grid) "row")
  (set-grid-row-count! grid (+ 1 (grid-row-count grid))))

(define (add-column! grid at)
  "Give the view a column for sheet column AT, renumbering the ones it displaces."
  (let ((index (make-column-index at)))
    (for-each (lambda (entry)
                (when (>= (column-index (car entry)) at)
                  (set-column-index! (car entry) (+ 1 (column-index (car entry))))))
              (grid-columns grid))
    (let ((view-column (make-column grid index "" %column-width)))
      (set-grid-columns! grid
                         (list-insert (grid-columns grid) at
                                      (cons index view-column)))
      ;; One past AT: the view's first column is the row gutter, which is no
      ;; column of the sheet.
      (gtk-column-view-insert-column (grid-view grid) (+ at 1) view-column))
    (reletter-columns! grid)))

(define (reletter-columns! grid)
  "Title every column with the letter its index now calls for."
  (for-each (lambda (entry)
              (gtk-column-view-column-set-title
               (cdr entry) (column->name (column-index (car entry)))))
            (grid-columns grid)))

(define (list-insert lst k item)
  (let loop ((rest lst) (i 0) (acc '()))
    (cond ((= i k) (append (reverse acc) (list item) rest))
          ((null? rest) (append (reverse acc) (list item)))
          (else (loop (cdr rest) (+ i 1) (cons (car rest) acc))))))

(define (grid-column-widths grid)
  "The columns whose width is not the default, as an alist of column index to
pixels.  Dragging the edge of a header sets the column's fixed width, so this
is what the user has actually done to the sheet -- and only that, so that a
sheet nobody has resized carries no widths at all."
  (filter-map (lambda (entry)
                (let ((width (gtk-column-view-column-get-fixed-width
                              (cdr entry))))
                  (and (not (= width %column-width))
                       (cons (column-index (car entry)) width))))
              (grid-columns grid)))

(define (grid-set-column-widths! grid widths)
  "Set the columns from WIDTHS, an alist of column index to pixels.  A column
the alist says nothing about, or nothing sensible, keeps the width it has."
  (for-each (lambda (entry)
              (let ((width (assv-ref widths (column-index (car entry)))))
                (when (and (integer? width) (> width 0))
                  (set-fixed-width (cdr entry) width))))
            (grid-columns grid)))

(define (grid-sync-size! grid)
  "Grow the view to the size of the sheet, which loading a file may have
changed.  Sheets only ever grow, so there is nothing here to take away."
  (let ((sheet (grid-sheet grid)))
    (let loop ()
      (when (< (grid-row-count grid) (sheet-rows sheet))
        (add-row! grid)
        (loop)))
    (let loop ()
      (let ((shown (length (grid-columns grid))))
        (when (< shown (sheet-columns sheet))
          (add-column! grid shown)
          (loop))))))


(define (scroll-to-row grid row)
  ;; gtk_column_view_scroll_to is GTK 4.12+; if the marshalling ever fails we
  ;; would rather not move the selection than crash the app.
  (catch #t
    (lambda ()
      (gtk-column-view-scroll-to (grid-view grid) row #f '() #f))
    (lambda args #f)))

(define (grid-focus! grid)
  (grab-focus (grid-view grid)))


;;;
;;; The context menu
;;;

;; Right-clicking a row number or a column header offers the four inserts.  The
;; menu items are the application's own actions and those work on the active
;; cell, so a right-click picks its line first, the way a left-click would:
;; what you point at is what you act on, and it is left selected afterwards so
;; you can see what happened.
;;
;; One popover serves the whole grid.  It is parented to the view rather than
;; to whatever was clicked, because the cell widgets under the pointer are
;; GtkColumnView's to recycle, and points at the click through the view's own
;; coordinates.

(define (install-line-menu! grid model)
  (when model
    (let ((menu (gtk-popover-menu-new-from-model model)))
      (set-has-arrow menu #f)
      (set-parent menu (grid-view grid))
      (set-grid-menu! grid menu))))

(define (install-line-click! grid widget axis locate)
  "Open the line menu when WIDGET is right-clicked.  LOCATE answers with the
row or column WIDGET stands for, or #f when it stands for nothing."
  (let ((gesture (gtk-gesture-click-new)))
    (gtk-gesture-single-set-button gesture 3)
    (connect gesture 'pressed
             (lambda (gesture n-press x y)
               (let ((index (locate)))
                 (when index
                   (gtk-gesture-set-state gesture 'claimed)
                   (select-line! grid axis index)
                   (open-line-menu grid widget x y)))))
    (add-controller widget gesture)))

(define (select-line! grid axis index)
  "Put the active cell on row or column INDEX, keeping the other half of the
reference where it is."
  (let ((active (grid-active grid)))
    (grid-set-active! grid
                      (if (eq? axis 'row)
                          (make-ref index (ref-column active))
                          (make-ref (ref-row active) index)))))

(define (open-line-menu grid widget x y)
  "Pop the menu up at X, Y in WIDGET."
  (let ((menu (grid-menu grid))
        (point (translate-point widget (grid-view grid) x y)))
    (when (and menu point)
      (gtk-popover-set-pointing-to menu (list (round-to-integer (first point))
                                              (round-to-integer (second point))
                                              1 1))
      (gtk-popover-popup menu))))

(define (round-to-integer x) (inexact->exact (round x)))


;;;
;;; Dragging a row or a column
;;;

;; GtkColumnView can reorder its own columns by dragging a header, but that
;; moves the view's columns and not the sheet behind them: the letters would
;; come out in the wrong order and A1 would no longer be the cell in the
;; corner. So the view's own reordering stays off (`reorderable: false' in
;; cellar.blp) and the drag is ours, ending in the same move-row! and
;; move-column! the keyboard uses.
;;
;; It is a GtkGestureDrag rather than GTK's drag-and-drop, because DnD is out
;; of reach from Guile: gdk_content_provider_new_for_value takes a GValue and
;; gtk_drop_target_new takes a GType, and G-Golf marshals neither. Nothing is
;; being transferred here anyway -- what is dragged never leaves the process --
;; so a gesture, which needs no content at all, is the better fit regardless.

;; Wide enough to keep clear of GTK's column resize handles, which live at the
;; edges of the same header widget the drag gesture is on.
(define %resize-margin 8)

(define (install-row-drag! grid cell label)
  (let ((gesture (gtk-gesture-drag-new)))
    (connect gesture 'drag-begin
             (lambda (gesture x y)
               (let ((position (get-position cell)))
                 (if (live-position? grid position)
                     (begin
                       (gtk-gesture-set-state gesture 'claimed)
                       (begin-drag! grid 'row position label x y))
                     (gtk-gesture-set-state gesture 'denied)))))
    (connect gesture 'drag-update
             (lambda (gesture offset-x offset-y)
               (update-drag! grid offset-x offset-y)))
    (connect gesture 'drag-end
             (lambda (gesture offset-x offset-y)
               (finish-drag! grid offset-x offset-y)))
    (connect gesture 'cancel
             (lambda (gesture sequence) (cancel-drag! grid)))
    (add-controller label gesture)))

(define (install-header-drag! grid)
  "Give each column header a drag gesture, and pair it with its column.

The headers are GtkColumnView's own widgets and there is no API that hands
them over -- a column has a title string, not a header factory -- so they are
reached by walking the view: its first child is the header row, whose children
are the column titles, the leading one belonging to the row gutter.

Which header belongs to which column cannot be read off that walk, because
GtkColumnView appends an inserted column's header to the end of the header row
even though the column itself went in somewhere in the middle.  What can be
relied on is that a header appears exactly once: so the headers already paired
keep their columns, and a header seen for the first time takes the column that
has not got one yet.  Headers come and go -- there are none until the view is
realised, and inserting a column adds one -- so this runs more than once."
  (let ((header (get-first-child (grid-view grid))))
    (when header
      (let* ((known (grid-titles grid))
             (gutter (get-first-child header))
             (widgets (if (not gutter)
                          '()
                          (let loop ((title (get-next-sibling gutter)) (acc '()))
                            (if (not title)
                                (reverse acc)
                                (loop (get-next-sibling title) (cons title acc))))))
             (kept (filter (lambda (entry) (memq (car entry) widgets)) known))
             (free (filter (lambda (entry)
                             (not (find (lambda (pair) (eq? (cdr pair) (car entry)))
                                        kept)))
                           (grid-columns grid))))
        (set-grid-titles!
         grid
         (let loop ((rest widgets) (free free) (titles '()))
           (cond
            ((null? rest) (reverse titles))
            ((assq (car rest) kept)
             => (lambda (entry) (loop (cdr rest) free (cons entry titles))))
            ((null? free) (reverse titles))
            (else
             (let ((index (car (car free))))
               (install-column-drag! grid (car rest) index)
               (loop (cdr rest) (cdr free)
                     (cons (cons (car rest) index) titles)))))))))))

(define (install-column-drag! grid title index)
  (let ((gesture (gtk-gesture-drag-new)))
    ;; Capture, not bubble. A header has gestures of GTK's own -- one of them
    ;; claims the sequence as soon as the pointer moves, and a bubble-phase
    ;; gesture here sees the press and then nothing at all.
    (set-propagation-phase gesture 'capture)
    (connect gesture 'drag-begin
             (lambda (gesture x y)
               ;; Near either edge the user is resizing the column, which is
               ;; GTK's gesture on this same widget. Stay out of its way.
               (if (< %resize-margin x (- (get-width title) %resize-margin))
                   (begin
                     (gtk-gesture-set-state gesture 'claimed)
                     (begin-drag! grid 'column (column-index index) title x y))
                   (gtk-gesture-set-state gesture 'denied))))
    (connect gesture 'drag-update
             (lambda (gesture offset-x offset-y)
               (update-drag! grid offset-x offset-y)))
    (connect gesture 'drag-end
             (lambda (gesture offset-x offset-y)
               (finish-drag! grid offset-x offset-y)))
    (connect gesture 'cancel
             (lambda (gesture sequence) (cancel-drag! grid)))
    (add-controller title gesture)
    (install-line-click! grid title 'column (lambda () (column-index index)))))

(define (begin-drag! grid axis from widget x y)
  (set-grid-drag! grid (make-drag axis from widget x y from))
  (show-drag grid))

(define (update-drag! grid offset-x offset-y)
  (let ((drag (grid-drag grid)))
    (when drag
      (let ((target (drag-target-at grid drag offset-x offset-y)))
        (when target
          (set-drag-target! drag target)
          (show-drag grid))))))

(define (finish-drag! grid offset-x offset-y)
  (let ((drag (grid-drag grid)))
    (when drag
      (let ((target (or (drag-target-at grid drag offset-x offset-y)
                        (drag-target drag))))
        (set-grid-drag! grid #f)
        (show-drag grid)
        (apply-move! grid (drag-axis drag) (drag-from drag) target)))))

(define (cancel-drag! grid)
  (set-grid-drag! grid #f)
  (show-drag grid))

(define (drag-target-at grid drag offset-x offset-y)
  "The row or column the pointer is over, OFFSET-X and OFFSET-Y into the drag."
  (let ((x (+ (drag-start-x drag) offset-x))
        (y (+ (drag-start-y drag) offset-y)))
    (if (eq? (drag-axis drag) 'row)
        (row-at grid (drag-widget drag) x y)
        (column-at grid (drag-widget drag) x))))

(define (row-at grid widget x y)
  (let ((point (translate-point widget (grid-view grid) x y)))
    (and point
         (let ((entry (cell-at grid (first point) (second point))))
           (and entry
                (let ((position (get-position (first entry))))
                  (and (live-position? grid position) position)))))))

(define (cell-at grid x y)
  "The (cell label column) entry under the point X, Y in view coordinates.

gtk_widget_pick answers with whatever widget is deepest at that point, which
is the cell widget GtkColumnView wraps our label in, so this looks at the
label under the answer as well as at the answer itself."
  (let loop ((widget (gtk-widget-pick (grid-view grid) x y '())) (depth 0))
    (and widget
         (< depth 3)
         (or (entry-for grid widget)
             (let ((child (get-first-child widget)))
               (and child (entry-for grid child)))
             (loop (get-parent widget) (+ depth 1))))))

(define (entry-for grid widget)
  (find (lambda (entry) (eq? (second entry) widget)) (grid-cells grid)))

(define (column-at grid widget x)
  "The column whose header contains X, given in WIDGET's coordinates.
Past the last header this is the last column, and before the first, the first:
a drag that overshoots means the end of the sheet, not nothing at all."
  (let* ((entries (headers-across grid))
         (header (and (pair? entries) (get-parent (car (car entries)))))
         (point (and header (translate-point widget header x 0.0))))
    (and point
         (let ((x (first point)))
           (let loop ((rest entries) (previous #f))
             (if (null? rest)
                 (and previous (column-index (cdr previous)))
                 (let* ((entry (car rest))
                        (origin (translate-point (car entry) header 0.0 0.0)))
                   (cond ((not origin) #f)
                         ((< x (first origin))
                          (column-index (cdr (or previous entry))))
                         ((< x (+ (first origin) (get-width (car entry))))
                          (column-index (cdr entry)))
                         (else (loop (cdr rest) entry))))))))))

(define (headers-across grid)
  "The (header . index) pairs in the order they lie across the screen, which
is the order the columns are in and not the order the header row holds them."
  (let ((entries (grid-titles grid)))
    (if (null? entries)
        entries
        (let ((header (get-parent (car (car entries)))))
          ;; (guile) sort: the bare name is a G-Golf generic here.
          ((@ (guile) sort)
           entries
           (lambda (a b)
             (let ((ax (translate-point (car a) header 0.0 0.0))
                   (bx (translate-point (car b) header 0.0 0.0)))
               (and ax bx (< (first ax) (first bx))))))))))

(define (translate-point widget target x y)
  "X and Y, given in WIDGET's coordinates, in TARGET's instead, or #f."
  (call-with-values
      (lambda () (gtk-widget-translate-coordinates widget target x y))
    (lambda (ok . point)
      (and ok (= (length point) 2) point))))


;;;
;;; What a drag looks like while it lasts
;;;

(define (show-drag grid)
  "Dim the line being dragged and light up the one it would land on."
  (let* ((drag (grid-drag grid))
         (row? (and drag (eq? (drag-axis drag) 'row)))
         (from (and drag (drag-from drag)))
         (target (and drag (drag-target drag))))
    (for-each
     (lambda (entry)
       (let* ((cell (first entry))
              (label (second entry))
              (column (column-index (third entry)))
              (position (get-position cell))
              ;; Which row or column this cell is in, on the axis being
              ;; dragged. The gutter is column -1, so it is never a column
              ;; target, but it is part of every row.
              (index (and drag
                          (live-position? grid position)
                          (if row? position column))))
         (set-css-class label "cellar-drag-source" (and index (= index from)))
         (set-css-class label "cellar-drag-target"
                        (and index target (= index target) (not (= target from))))))
     (grid-cells grid))
    (for-each
     (lambda (entry)
       (let ((title (car entry))
             (column (column-index (cdr entry)))
             (column? (and drag (not row?))))
         (set-css-class title "cellar-drag-source" (and column? (= column from)))
         (set-css-class title "cellar-drag-target"
                        (and column? target (= column target) (not (= target from))))))
     (grid-titles grid))))

(define (set-css-class widget class on?)
  (if on?
      (add-css-class widget class)
      (remove-css-class widget class)))


;;;
;;; Keyboard
;;;

(define %key-return 65293)
(define %key-kp-enter 65421)
(define %key-left 65361)
(define %key-up 65362)
(define %key-right 65363)
(define %key-down 65364)
(define %key-page-up 65365)
(define %key-page-down 65366)
(define %key-home 65360)
(define %key-end 65367)
(define %key-tab 65289)
(define %key-iso-left-tab 65056)
(define %key-delete 65535)
(define %key-backspace 65288)

(define (install-key-handling grid)
  (let ((controller (make <gtk-event-controller-key>)))
    ;; Capture, not bubble: GtkColumnView binds the arrow keys for its own row
    ;; navigation, and in the bubble phase it would swallow them before we ever
    ;; saw them. There are no editable widgets inside the grid, so taking keys
    ;; first is safe.
    (set-propagation-phase controller 'capture)
    (connect controller 'key-pressed
             (lambda (controller keyval keycode state)
               (key-pressed grid keyval state)))
    (add-controller (grid-view grid) controller)))

(define (key-pressed grid keyval state)
  "Return #t to stop the key from propagating."
  (let ((shift? (memq 'shift-mask state)))
    (cond
     ((= keyval %key-left) (grid-move-active! grid 0 -1) #t)
     ((= keyval %key-right) (grid-move-active! grid 0 1) #t)
     ((= keyval %key-up) (grid-move-active! grid -1 0) #t)
     ((= keyval %key-down) (grid-move-active! grid 1 0) #t)
     ((= keyval %key-page-up) (grid-move-active! grid -10 0) #t)
     ((= keyval %key-page-down) (grid-move-active! grid 10 0) #t)
     ((= keyval %key-home) (grid-move-active! grid 0 -1000) #t)
     ((= keyval %key-end) (grid-move-active! grid 0 1000) #t)
     ((= keyval %key-tab) (grid-move-active! grid 0 1) #t)
     ((= keyval %key-iso-left-tab) (grid-move-active! grid 0 -1) #t)
     ((or (= keyval %key-return) (= keyval %key-kp-enter))
      ((grid-on-activate grid) (grid-active grid))
      #t)
     ((or (= keyval %key-delete) (= keyval %key-backspace))
      (set-cell-source! (grid-sheet grid) (grid-active grid) "")
      (grid-refresh! grid)
      ((grid-on-select grid) (grid-active grid))
      #t)
     (else #f))))
