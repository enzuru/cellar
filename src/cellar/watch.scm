;;; Cellar -- noticing that the workbook on disk has changed.
;;;
;;; A workbook is a folder of ordinary files, so anything at all can change it:
;;; an editor, a `git checkout', another copy of Cellar.  This watches the
;;; folder and says when something did.
;;;
;;; It says only that, never what: the caller re-reads the whole workbook and
;;; compares.  That sounds wasteful and is not -- a workbook is a few dozen
;;; small files -- and it buys the property that makes this safe, which is that
;;; Cellar's own writes are indistinguishable from anyone else's.  Our writes
;;; land, the watcher fires, the caller finds the disk already says what the
;;; model says, and nothing happens.  There is no need to remember which files
;;; we wrote, and so no way to get that bookkeeping wrong.
;;;
;;; What is watched is a list of paths rather than one folder, because a
;;; workbook is several sheets and a sheet is a folder and a file.  A path that
;;; is not there yet is skipped rather than refused: sheets/ does not exist in
;;; a workbook written before there were tabs, and that workbook still has to
;;; be watchable.

(define-module (cellar watch)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:use-module (cellar gi)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:duplicates (merge-generics replace warn-override-core warn last)
  #:export (watch-paths!
            unwatch!))

;; How long to wait for a burst of changes to finish before believing it.
;; Writing a file is several inotify events, and a `git checkout' of a sheet is
;; several files; both should cost one re-read, not a dozen.
(define %settle-milliseconds 250)

(define-record-type <watcher>
  (make-watcher monitors pending)
  watcher?
  (monitors watcher-monitors set-watcher-monitors!)
  (pending watcher-pending set-watcher-pending!))

(define (watch-paths! paths on-change)
  "Watch every path in PATHS and call ON-CHANGE once each time they settle.
Returns a watcher to hand to `unwatch!', or #f if nothing could be watched --
which is not fatal, and leaves the workbook working exactly as it did before,
only without noticing edits made behind its back."
  (catch #t
    (lambda ()
      ;; One record, made first and filled in after: the handlers below close
      ;; over it, and a second copy would leave `unwatch!' cancelling a timeout
      ;; that the handlers were not using.
      (let ((watcher (make-watcher '() #f)))
        (set-watcher-monitors!
         watcher
         (filter-map
          (lambda (path)
            (monitor-path path (lambda () (settle! watcher on-change))))
          paths))
        (and (pair? (watcher-monitors watcher)) watcher)))
    (lambda arguments #f)))

(define (monitor-path path on-event)
  "Monitor PATH, whether it is a folder of cells or a single file.  A path that
does not exist is watched as a file, which is how a workbook notices one being
created."
  (catch #t
    (lambda ()
      (let* ((file (g-file-new-for-path path))
             (monitor (if (and (file-exists? path) (directory? path))
                          (g-file-monitor-directory file '() #f)
                          (g-file-monitor-file file '() #f))))
        ;; The signal carries which file changed and how, and we want neither:
        ;; the caller re-reads everything regardless, so one code path covers a
        ;; created file, a written one and a deleted one alike.
        (connect monitor 'changed
                 (lambda (monitor file other event) (on-event)))
        monitor))
    (lambda arguments #f)))

(define (directory? path)
  (catch #t
    (lambda () (eq? 'directory (stat:type (stat path))))
    (lambda arguments #f)))

(define (settle! watcher on-change)
  "Restart the quiet period.  ON-CHANGE runs once the changes stop coming."
  (let ((pending (watcher-pending watcher)))
    (when pending (g-source-remove pending)))
  (set-watcher-pending!
   watcher
   (g-timeout-add %settle-milliseconds
                  (lambda ()
                    (set-watcher-pending! watcher #f)
                    (on-change)
                    ;; #f: a one-shot timeout, not a heartbeat.
                    #f))))

(define (unwatch! watcher)
  "Stop watching.  Safe to call with #f, and safe to call twice."
  (when (watcher? watcher)
    (let ((pending (watcher-pending watcher)))
      (when pending
        (g-source-remove pending)
        (set-watcher-pending! watcher #f)))
    (for-each (lambda (monitor)
                (catch #t
                  (lambda () (g-file-monitor-cancel monitor))
                  (lambda arguments #f)))
              (watcher-monitors watcher))))
