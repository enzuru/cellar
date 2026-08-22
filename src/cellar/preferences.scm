;;; Cellar -- the preferences dialog.
;;;
;;; Two controls: whether cells go to an external editor, and what to run when
;;; they do. Changes take effect as you make them and are written to disk when
;;; the dialog closes.

(define-module (cellar preferences)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar config)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (open-preferences))

(define (open-preferences ui-directory parent)
  "Present the preferences over PARENT."
  (let* ((builder (make <gtk-builder>))
         (path (string-append ui-directory "/preferences.ui")))
    (when (eqv? 0 (add-from-file builder path))
      (error "cellar: could not load the preferences UI:" path))
    (let ((dialog (get-object builder "preferences_dialog"))
          (group (get-object builder "editor_group"))
          (switch (get-object builder "external_editor_switch"))
          (entry (get-object builder "external_editor_command"))
          (hint (get-object builder "command_hint"))
          (override (get-object builder "override_row")))

      (set-active switch (external-editor-enabled?))
      (set-text entry (external-editor-command))

      (define (retune)
        "The command only matters when the switch is on."
        (let ((on (get-active switch)))
          (set-sensitive entry on)
          (set-sensitive hint on)))

      (connect switch 'notify::active
               (lambda (row parameter)
                 (set-external-editor-enabled! (get-active row))
                 (retune)))
      (connect entry 'changed
               (lambda (row) (set-external-editor-command! (get-text row))))

      ;; An environment variable beats the saved preference, and a dialog that
      ;; quietly did nothing would be a mystery; say what is happening instead.
      (let ((forced (editor-override)))
        (when forced
          (set-visible override #t)
          (set-subtitle override
                        (if (eq? forced 'internal)
                            "CELLAR_EDITOR is set to nothing, so this run uses the built-in editor whatever you choose here."
                            (format #f "This run edits cells with `~a' whatever you choose here." forced)))))

      (retune)
      (connect dialog 'closed (lambda (dialog) (save-config!)))
      (present dialog parent)
      dialog)))
