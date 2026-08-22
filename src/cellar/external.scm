;;; Cellar -- handing a cell to an external editor.
;;;
;;; A cell is a file in the sheet folder, so there is nothing to hand over but
;;; its path: the editor opens cells/B2.scm, and saving in the editor saves the
;;; cell.  Cellar does not wait for the editor to exit and does not read
;;; anything back -- the sheet folder is watched, so the grid catches up with
;;; each save on its own, with the editor still open.
;;;
;;; This is why an external editor no longer has to be told to wait.  An
;;; earlier version copied the cell to a temporary file and read it back when
;;; the editor exited, which meant `code' or `gedit' had to be run with
;;; --wait or the edit was lost.  Now the file is the cell.

(define-module (cellar external)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (cellar config)
  #:use-module (cellar model)
  #:use-module (cellar store)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (open-external-editor))

;; Editors still running.  Nothing is waiting on them, but a GSubprocess that
;; nothing holds is a GSubprocess that may be collected out from under the
;; running program, so they are kept until they exit.
(define *running* '())

(define (open-external-editor command directory r on-error)
  "Open cell R of the sheet at DIRECTORY with COMMAND.  Returns #t if the
editor started, #f -- having called ON-ERROR with a message -- if it could not,
so the caller can fall back to the built-in editor.

Nothing is applied here.  The editor writes the cell's own file, and the sheet
watcher is what notices."
  (let* ((path (cell-file-path directory (ref->name r)))
         (argv (editor-argv command path)))
    (if (null? argv)
        (begin (on-error "The external editor command is empty") #f)
        (catch #t
          (lambda ()
            (let ((process (g-subprocess-new argv '())))
              (set! *running* (cons process *running*))
              (g-subprocess-wait-async
               process #f
               (lambda (source result data)
                 (set! *running* (delq! process *running*)))
               #f)
              #t))
          (lambda arguments
            ;; Nearly always a command that is not on PATH.  Say which, and let
            ;; the caller open the built-in editor rather than leaving a cell
            ;; that cannot be edited at all.
            (on-error (format #f "Could not start ~a" (car argv)))
            #f)))))
