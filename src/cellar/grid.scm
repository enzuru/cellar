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
            grid-focus!))

;; GTK_INVALID_LIST_POSITION -- what get-position returns for an unbound cell.
(define %invalid-position #xffffffff)

(define %column-width 104)
(define %gutter-width 60)

(define-record-type <grid>
  (%make-grid view sheet cells active on-select on-activate)
  grid?
  (view grid-view)
  (sheet grid-sheet)
  ;; Every (cell label column) triple handed to us by a factory `setup'.
  ;; Cell widgets are recycled, so this list stays proportional to the visible
  ;; area, not to the size of the sheet.
  (cells grid-cells set-grid-cells!)
  (active grid-active %set-grid-active!)
  (on-select grid-on-select)
  (on-activate grid-on-activate))

(define (make-grid view sheet on-select on-activate)
  "Turn VIEW, a GtkColumnView, into a grid over SHEET.
ON-SELECT is called with a reference whenever the active cell changes.
ON-ACTIVATE is called with a reference when a cell is double-clicked or
activated from the keyboard."
  (let ((grid (%make-grid view sheet '() (make-ref 0 0) on-select on-activate)))
    (set-model view (gtk-no-selection-new (row-model sheet)))
    ;; A narrow leading column of row numbers, standing in for the row headers
    ;; GtkColumnView does not have.
    (append-column view (make-column grid "" -1 %gutter-width))
    (for-each (lambda (column)
                (append-column view
                               (make-column grid
                                            (column->name column)
                                            column
                                            %column-width)))
              (iota (sheet-columns sheet)))
    (install-key-handling grid)
    grid))

(define (row-model sheet)
  "A GtkStringList with one entry per row. The strings are never read; the
model exists only to give the view its row count."
  (let ((model (make <gtk-string-list>)))
    (for-each (lambda (row) (append model (number->string row)))
              (iota (sheet-rows sheet)))
    model))

(define (make-column grid title column width)
  (let ((factory (make <gtk-signal-list-item-factory>)))
    (connect factory 'setup
             (lambda (factory cell) (setup-cell grid column cell)))
    (connect factory 'bind
             (lambda (factory cell) (bind-cell grid column cell)))
    (let ((view-column (gtk-column-view-column-new title factory)))
      (set-fixed-width view-column width)
      (set-resizable view-column (>= column 0))
      view-column)))


;;;
;;; Cell widgets
;;;

(define (setup-cell grid column cell)
  (let ((label (make <gtk-label>
                 #:hexpand #t
                 #:xalign (if (< column 0) 0.5 0.0)
                 #:ellipsize 'end
                 #:single-line-mode #t
                 #:margin-start 6
                 #:margin-end 6)))
    (add-css-class label (if (< column 0) "cellar-gutter" "cellar-cell"))
    (set-child cell label)
    (when (>= column 0)
      (let ((gesture (gtk-gesture-click-new)))
        (connect gesture 'pressed
                 (lambda (gesture n-press x y)
                   (cell-pressed grid column cell n-press)))
        (add-controller label gesture)))
    (set-grid-cells! grid (cons (list cell label column) (grid-cells grid)))))

(define (bind-cell grid column cell)
  (let ((position (get-position cell)))
    (when (live-position? grid position)
      (paint-cell grid (get-child cell) column position))))

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
            (remove-css-class label "cellar-active")))))

(define (grid-refresh! grid)
  "Repaint every realised cell. Called after the sheet changes."
  (for-each (lambda (entry)
              (let ((cell (first entry))
                    (label (second entry))
                    (column (third entry)))
                (let ((position (get-position cell)))
                  (when (live-position? grid position)
                    (paint-cell grid label column position)))))
            (grid-cells grid)))


;;;
;;; Selection
;;;

(define (cell-pressed grid column cell n-press)
  (let ((position (get-position cell)))
    (when (live-position? grid position)
      (let ((r (make-ref position column)))
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
;; grid does is ask for it and keep the active cell on the line that moved.
(define (grid-move-line! grid axis delta)
  "Move the active cell's row (AXIS 'row) or column (AXIS 'column) DELTA places.
Returns #t when the sheet changed."
  (let* ((sheet (grid-sheet grid))
         (active (grid-active grid))
         (row? (eq? axis 'row))
         (from (if row? (ref-row active) (ref-column active)))
         (to (+ from delta)))
    (and (if row? (move-row! sheet from to) (move-column! sheet from to))
         (let ((moved (if row?
                          (make-ref to (ref-column active))
                          (make-ref (ref-row active) to))))
           (grid-set-active! grid moved)
           (scroll-to-row grid (ref-row moved))
           #t))))

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
