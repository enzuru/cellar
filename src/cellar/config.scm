;;; Cellar -- preferences that outlive the session.
;;;
;;; One small alist, written to $XDG_CONFIG_HOME/cellar/config.scm. It holds the
;;; only preference Cellar has: whether to hand cells to an external editor
;;; instead of the built-in one, and what to run when it does.
;;;
;;; Like (cellar model), this module knows nothing about GTK, so the command
;;; parsing below can be tested without a display.

(define-module (cellar config)
  #:use-module (ice-9 match)
  #:export (config-file-path
            load-config!
            save-config!
            external-editor-enabled?
            set-external-editor-enabled!
            external-editor-command
            set-external-editor-command!
            editor-override
            effective-editor-command
            split-command
            editor-argv))

(define %defaults
  '((external-editor-enabled . #f)
    (external-editor-command . "")))

(define *config* %defaults)

(define (config-file-path)
  "Where the preferences live. CELLAR_CONFIG overrides it, which is how the
tests get a config file of their own."
  (or (getenv "CELLAR_CONFIG")
      (string-append (or (getenv "XDG_CONFIG_HOME")
                         (string-append (or (getenv "HOME") ".") "/.config"))
                     "/cellar/config.scm")))

(define (alist-set alist key value)
  "ALIST with KEY bound to VALUE, without touching ALIST itself -- %defaults is a
literal, and literals in Guile are not ours to mutate."
  (cons (cons key value)
        (filter (lambda (entry) (not (eq? (car entry) key))) alist)))

(define (setting key)
  (assq-ref *config* key))

(define (set-setting! key value)
  (set! *config* (alist-set *config* key value)))

(define (external-editor-enabled?)
  (and (setting 'external-editor-enabled) #t))

(define (set-external-editor-enabled! enabled?)
  (set-setting! 'external-editor-enabled (and enabled? #t)))

(define (external-editor-command)
  (let ((command (setting 'external-editor-command)))
    (if (string? command) command "")))

(define (set-external-editor-command! command)
  (set-setting! 'external-editor-command (if (string? command) command "")))


;;;
;;; Reading and writing
;;;

(define (load-config!)
  "Read the preferences back. A missing file is the ordinary first-run case and a
corrupt one is not worth refusing to start over; either way we keep the
defaults."
  (set! *config* %defaults)
  (catch #t
    (lambda ()
      (let ((path (config-file-path)))
        (when (file-exists? path)
          (let ((data (call-with-input-file path read)))
            (when (list? data)
              (set! *config* (overlay data %defaults)))))))
    (lambda arguments #f)))

(define (overlay data base)
  "BASE with each (key . value) of DATA laid over it. Keys we do not recognise
are dropped, so an old or hand-edited file cannot cost us a default."
  (let loop ((entries data) (result base))
    (match entries
      (() result)
      (((key . value) . rest)
       (loop rest (if (assq key %defaults) (alist-set result key value) result)))
      ((_ . rest) (loop rest result)))))

(define (save-config!)
  "Write the preferences out, creating ~/.config/cellar if it is not there yet."
  (let* ((path (config-file-path))
         (directory (dirname path)))
    (unless (file-exists? directory)
      (mkdir-p directory))
    (call-with-output-file path
      (lambda (port)
        (display ";; Cellar preferences.\n" port)
        (write *config* port)
        (newline port)))))

(define (mkdir-p directory)
  (let loop ((parts (filter (lambda (part) (not (string-null? part)))
                            (string-split directory #\/)))
             (so-far (if (string-prefix? "/" directory) "" ".")))
    (match parts
      (() #t)
      ((part . rest)
       (let ((next (string-append so-far "/" part)))
         (unless (file-exists? next) (mkdir next #o755))
         (loop rest next))))))


;;;
;;; Which editor to use
;;;

(define (blank->false string)
  (and (string? string)
       (not (string-null? (string-trim-both string)))
       (string-trim-both string)))

(define (editor-override)
  "The command CELLAR_EDITOR asks for, 'internal if it is set but empty, or #f
if it is not set at all."
  (let ((value (getenv "CELLAR_EDITOR")))
    (cond ((not value) #f)
          ((blank->false value))
          (else 'internal))))

(define (effective-editor-command)
  "The command to run instead of the built-in editor, or #f to use the built-in
one. CELLAR_EDITOR wins over the saved preference: set it to a command to force
an external editor for one run, or to the empty string to force the built-in
one."
  (match (editor-override)
    ('internal #f)
    (#f (and (external-editor-enabled?)
             (blank->false (external-editor-command))))
    (command command)))


;;;
;;; Turning a command into an argument vector
;;;

(define (split-command command)
  "Split COMMAND into arguments the way a shell would: single quotes take
everything literally, double quotes group without hiding backslashes, and a
backslash outside single quotes escapes the character after it."
  (let loop ((chars (string->list command))
             (current '())
             (started? #f)
             (quotation #f)
             (arguments '()))
    (define (emit)
      (cons (list->string (reverse current)) arguments))
    (match chars
      (() (reverse (if started? (emit) arguments)))
      ((#\\ next . rest)
       (if (eqv? quotation #\')
           (loop (cons next rest) (cons #\\ current) #t quotation arguments)
           (loop rest (cons next current) #t quotation arguments)))
      ((char . rest)
       (cond
        ((and (not quotation) (or (char=? char #\") (char=? char #\')))
         (loop rest current #t char arguments))
        ((eqv? quotation char)
         (loop rest current #t #f arguments))
        ((and (not quotation) (char-whitespace? char))
         (if started?
             (loop rest '() #f #f (emit))
             (loop rest '() #f #f arguments)))
        (else
         (loop rest (cons char current) #t quotation arguments)))))))

(define (substitute-path token path)
  "TOKEN with every %s replaced by PATH and every %% by a literal %. Returns the
new token and whether a %s was there at all."
  (let loop ((chars (string->list token)) (out '()) (used? #f))
    (match chars
      (() (values (list->string (reverse out)) used?))
      ((#\% #\s . rest)
       (loop rest (append (reverse (string->list path)) out) #t))
      ((#\% #\% . rest) (loop rest (cons #\% out) used?))
      ((char . rest) (loop rest (cons char out) used?)))))

(define (editor-argv command path)
  "The argument vector that runs COMMAND on PATH. A %s anywhere in the command
becomes the file name -- `xterm -e vim %s' -- and without one the file is added
at the end, which is what a plain `gnome-text-editor' wants. Substitution happens
after splitting, so a path with spaces in it stays a single argument."
  (let loop ((tokens (split-command command)) (out '()) (used? #f))
    (match tokens
      (()
       (let ((argv (reverse out)))
         (cond ((null? argv) '())
               (used? argv)
               (else (append argv (list path))))))
      ((token . rest)
       (call-with-values (lambda () (substitute-path token path))
         (lambda (substituted seen?)
           (loop rest (cons substituted out) (or used? seen?))))))))
