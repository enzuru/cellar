;;; Cellar -- the cell editor.
;;;
;;; Double-clicking a cell opens this: a GtkSourceView with Scheme highlighting
;;; in an AdwDialog. Whatever you type is the cell's source, and the Result line
;;; underneath evaluates it as you type -- against the real sheet, but without
;;; committing anything, so a half-written expression never corrupts the grid.

(define-module (cellar editor)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar model)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (open-cell-editor))

(define %key-return 65293)
(define %key-kp-enter 65421)

(define (open-cell-editor ui-directory parent sheet r on-apply)
  "Present the editor for cell R over PARENT. ON-APPLY is called with the new
source text when the user applies the edit."
  (let* ((builder (make <gtk-builder>))
         (path (string-append ui-directory "/editor.ui")))
    (when (eqv? 0 (add-from-file builder path))
      (error "cellar: could not load the editor UI:" path))
    (let* ((dialog (get-object builder "editor_dialog"))
           (title (get-object builder "editor_title"))
           (view (get-object builder "source_view"))
           (buffer (get-object builder "source_buffer"))
           (result (get-object builder "result_label"))
           (cancel (get-object builder "cancel_button"))
           (apply-button (get-object builder "apply_button")))

      (set-title title (format #f "Cell ~a" (ref->name r)))
      (set-subtitle title (describe-cell sheet r))
      (apply-syntax-highlighting buffer)
      (set-text buffer (or (cell-source sheet r) "") -1)

      (let ((commit
             (lambda ()
               (on-apply (buffer-text buffer))
               (adw-dialog-close dialog))))

        (connect buffer 'changed
                 (lambda (buffer) (show-result sheet r buffer result)))
        (connect cancel 'clicked (lambda (button) (adw-dialog-close dialog)))
        (connect apply-button 'clicked (lambda (button) (commit)))

        ;; Ctrl+Return applies, so you never have to reach for the mouse.
        (let ((keys (make <gtk-event-controller-key>)))
          (connect keys 'key-pressed
                   (lambda (controller keyval keycode state)
                     (if (and (or (= keyval %key-return) (= keyval %key-kp-enter))
                              (memq 'control-mask state))
                         (begin (commit) #t)
                         #f)))
          (add-controller view keys)))

      (show-result sheet r buffer result)
      (present dialog parent)
      (grab-focus view)
      dialog)))

(define (describe-cell sheet r)
  (if (cell-source sheet r)
      "Editing an existing cell"
      "This cell is empty"))

(define (buffer-text buffer)
  (let ((start (get-start-iter buffer))
        (end (get-end-iter buffer)))
    (get-text buffer start end #f)))

(define (show-result sheet r buffer label)
  "Evaluate the buffer against the sheet and report the result, without
committing the edit."
  (let* ((text (buffer-text buffer))
         (value (if (string-null? (string-trim-both text))
                    #f
                    (preview-source sheet r text))))
    (cond
     ((not value)
      (set-label label "empty")
      (add-css-class label "dim-label")
      (remove-css-class label "error"))
     ((cell-error? value)
      (set-label label (cell-error-message value))
      (remove-css-class label "dim-label")
      (add-css-class label "error"))
     (else
      (set-label label (cell-display-value value))
      (remove-css-class label "dim-label")
      (remove-css-class label "error")))))

(define (cell-display-value value)
  "Show the result the way the grid will, so the preview and the cell agree.
Strings are the exception: they are written with their quotes, so that the
string \"12\" is visibly not the number 12."
  (if (string? value)
      (with-output-to-string (lambda () (write value)))
      (format-value value)))

(define (apply-syntax-highlighting buffer)
  "Scheme highlighting, in a style scheme that matches the current light/dark
preference."
  (catch #t
    (lambda ()
      (let ((language (gtk-source-language-manager-get-language
                        (gtk-source-language-manager-get-default) "scheme")))
        (when language (gtk-source-buffer-set-language buffer language)))
      (let* ((dark (get-dark (adw-style-manager-get-default)))
             (schemes (gtk-source-style-scheme-manager-get-default))
             (scheme (or (gtk-source-style-scheme-manager-get-scheme schemes (if dark "Adwaita-dark" "Adwaita"))
                         (gtk-source-style-scheme-manager-get-scheme schemes (if dark "solarized-dark" "solarized-light")))))
        (when scheme (gtk-source-buffer-set-style-scheme buffer scheme))))
    (lambda args
      ;; Highlighting is a nicety; never let it stop the editor from opening.
      #f)))
