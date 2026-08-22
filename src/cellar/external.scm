;;; Cellar -- handing a cell to an external editor.
;;;
;;; The cell's source goes into a file of its own, the configured command is
;;; started on it, and when that command exits the file comes back as the cell.
;;; The wait is asynchronous, so the sheet stays usable while an editor is open
;;; and several cells can be out with editors at the same time.

(define-module (cellar external)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (ice-9 textual-ports)
  #:use-module (cellar gi)
  #:use-module (cellar config)
  #:use-module (cellar model)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (open-external-editor))

;; Editors that are still open. A pending job holds the GSubprocess and the
;; callback that is waiting on it, so neither can be collected while the user is
;; still typing in another window.
(define *pending* '())

(define (open-external-editor command sheet r on-apply on-error)
  "Edit cell R with COMMAND. ON-APPLY is called with the file's contents once
the editor exits cleanly; ON-ERROR is called with a message if the editor cannot
be started or exits badly. Returns #t if the editor started, #f if the caller
should fall back to the built-in one."
  (let* ((directory (make-temporary-directory))
         (path (and directory
                    (string-append directory "/" (ref->name r) ".scm")))
         (argv (and path (editor-argv command path))))
    (cond
     ((not path)
      (on-error "Could not make a temporary file for the external editor")
      #f)
     ((null? argv)
      (discard directory)
      (on-error "The external editor command is empty")
      #f)
     (else
      (catch #t
        (lambda ()
          (call-with-output-file path
            (lambda (port) (display (or (cell-source sheet r) "") port))))
        (lambda arguments #f))
      (start argv directory path on-apply on-error)))))

(define (start argv directory path on-apply on-error)
  (catch #t
    (lambda ()
      (let* ((process (g-subprocess-new argv '()))
             (job #f))
        (define (done ok?)
          (set! *pending* (delq! job *pending*))
          (let ((text (and ok? (read-back path))))
            (discard directory)
            (cond (text (on-apply text))
                  (ok? (on-error "The external editor left no file behind"))
                  (else (on-error "The external editor did not finish cleanly; the cell is unchanged")))))
        (set! job (cons process
                        (lambda (source result data)
                          (done (catch #t
                                  (lambda ()
                                    (g-subprocess-wait-check-finish source result))
                                  (lambda arguments #f))))))
        (set! *pending* (cons job *pending*))
        (g-subprocess-wait-check-async process #f (cdr job) #f)
        #t))
    (lambda arguments
      ;; The usual cause is a command that is not on PATH. Say so and let the
      ;; caller fall back rather than leaving the cell uneditable.
      (discard directory)
      (on-error (format #f "Could not start ~a" (car argv)))
      #f)))

(define (read-back path)
  "The contents of PATH, or #f if it is gone -- an editor that deleted the file
is telling us the edit was abandoned."
  (catch #t
    (lambda ()
      (and (file-exists? path)
           (call-with-input-file path get-string-all)))
    (lambda arguments #f)))

(define (make-temporary-directory)
  "A directory of our own under TMPDIR, so the file inside it can be named after
the cell -- `A1.scm', which an editor will highlight as Scheme."
  (catch #t
    (lambda ()
      (mkdtemp (string-append (or (getenv "TMPDIR") "/tmp") "/cellar-XXXXXX")))
    (lambda arguments #f)))

(define (discard directory)
  (when directory
    (catch #t
      (lambda ()
        (for-each (lambda (entry)
                    (delete-file (string-append directory "/" entry)))
                  (scandir* directory))
        (rmdir directory))
      (lambda arguments #f))))

(define (scandir* directory)
  (let ((stream (opendir directory)))
    (let loop ((entry (readdir stream)) (entries '()))
      (cond ((eof-object? entry) (closedir stream) entries)
            ((or (string=? entry ".") (string=? entry ".."))
             (loop (readdir stream) entries))
            (else (loop (readdir stream) (cons entry entries)))))))
