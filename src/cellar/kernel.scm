;;; Cellar -- the half that owns the sheets.
;;;
;;; The kernel holds the sheets and evaluates them, and does nothing else.  It
;;; has no window, no files, and no opinion about tabs; it is handed cells as
;;; text and asked what they come to.
;;;
;;; Why it is its own process
;;;
;;; A cell is an arbitrary Guile expression, so a cell can be `(let loop ()
;;; (loop))'.  While the model lived inside the application that meant the
;;; window stopped -- evaluation ran inside the paint, and there was nowhere to
;;; catch it from.  Out here it means one process stops, and the one holding the
;;; window is still drawing, still has the last thing the kernel said, and can
;;; put a stop to it.
;;;
;;; The other reason is the shape of what crosses the wire.  The shell never
;;; asks what a single cell comes to; it is told the whole sheet at once, as
;;; strings that are ready to draw.  So the paint path does no talking, and the
;;; grid can be repainted a hundred times over an answer that was computed once.
;;;
;;; What it never does
;;;
;;; The kernel does not touch the disk.  Everything it knows arrives in an
;;; `open' and leaves in a `sources', which is what keeps the store on the other
;;; side of the wire where the window is -- and what will let the store be
;;; rewritten in another language without this file noticing.

(define-module (cellar kernel)
  #:use-module (cellar model)
  #:use-module (cellar ref)
  #:use-module (cellar protocol)
  #:use-module (ice-9 match)
  #:export (make-kernel
            kernel-serve
            run-kernel))

;; The kernel is a book: the sheets the shell has open, by the names on their
;; tabs.  It knows the names because cells use them -- a cell that says
;; Summary!B2 is asking for a sheet by name, and something has to know which
;; one that is.  Nothing else about a tab is any of its business, and a rename
;; is a request like any other.
(define (make-kernel) (make-book))


;;;
;;; Serving one request
;;;

(define (kernel-serve kernel message)
  "The reply MESSAGE deserves.  Never throws: a request that cannot be served
comes back as a `fail', because the shell is drawing a window and an exception
here would take the window's answers away with it."
  (match message
    (('request id op . arguments)
     (catch #t
       (lambda () (serve kernel id op arguments))
       (lambda (key . args)
         (list 'fail id (describe key args)))))
    (('unreadable) (list 'fail 0 "a message that could not be read at all"))
    (_ (list 'fail 0 (format #f "not a request: ~s" message)))))

(define (describe key args)
  (if (and (eq? key 'cellar-kernel-error) (pair? args) (string? (car args)))
      (car args)
      (format #f "~a ~s" key args)))

(define (serve kernel id op arguments)
  (match (cons op arguments)

    (('ping) (reply id '()))

    ;; The shell has read a sheet off the disk and is handing it over.  Opening
    ;; the same name twice replaces what was there, which is what a reload is.
    (('open sheet rows columns cells)
     ;; Exactly the size asked for, and no opinion about what a reasonable size
     ;; would be.  How much empty room a new sheet gets is a question about what
     ;; looks right in a window, and the window is not here.
     (let ((s (open-sheet! kernel (sheet-name-given sheet)
                           (max 1 rows) (max 1 columns))))
       (alist->sheet! s cells)
       (grow-sheet! s rows columns)
       (reply id (with-others kernel s (snapshot-of s) #f))))

    (('close sheet)
     (close-sheet! kernel sheet)
     ;; The sheets that are left may have been reading the one that has gone,
     ;; so they go back as well.
     (reply id (with-others kernel #f '() #f)))

    ;; A tab has been renamed.  The cells have not moved, but every reference
    ;; that named the sheet has, so the sources of every sheet go back with the
    ;; answer -- the shell has files to bring into line.
    (('rename from to)
     (let ((s (sheet-called kernel from))
           (wanted (sheet-name-given to)))
       (when (and (not (equal? from wanted)) (book-sheet kernel wanted))
         (throw 'cellar-kernel-error
                (format #f "a sheet called ~s is already open" wanted)))
       (rename-sheet! kernel from wanted)
       (reply id (with-others kernel s (with-sources s) #t))))

    (('set-cell sheet name source)
     (let* ((s (sheet-called kernel sheet))
            (r (reference name)))
       (set-cell-source! s r source)
       ;; The source is echoed back as the model now holds it -- trimmed, or
       ;; #f for a cell that has been emptied.  The shell writes files, and
       ;; what it writes should be what is true here rather than its own guess
       ;; at what trimming means.
       (reply id (with-others kernel s
                              (cons (cons 'source (cell-source s r))
                                    (snapshot-of s))
                              #f))))

    (('preview sheet name source)
     ;; The editor's live result, which is evaluated without being kept.
     (let* ((s (sheet-called kernel sheet))
            (r (reference name))
            (value (preview-source s r source)))
       (reply id `((display . ,(if (cell-error? value)
                                   (cell-error-message value)
                                   (format-value value)))
                   ;; The editor shows a string with its quotes on, so that
                   ;; the string "12" is visibly not the number 12; the grid
                   ;; shows it without.  Both forms go back, because working
                   ;; the second one out from the first is not possible and
                   ;; the shell has no evaluator to ask.
                   (written . ,(cond ((cell-error? value)
                                      (cell-error-message value))
                                     ((string? value)
                                      (with-output-to-string
                                        (lambda () (write value))))
                                     (else (format-value value))))
                   (error . ,(and (cell-error? value) #t))))))

    ;; Moving and inserting rewrite the references inside other cells -- on
    ;; other sheets too, since a reference to a moved row is a reference to it
    ;; wherever it is written -- so the sources go back with the snapshot: the
    ;; shell has files to bring into line and its copy of the text is now
    ;; stale.
    (('move sheet axis from to)
     (let ((s (sheet-called kernel sheet)))
       (unless (if (eq? axis 'row)
                   (move-row! s from to)
                   (move-column! s from to))
         (throw 'cellar-kernel-error "that line is already at the edge"))
       (reply id (with-others kernel s (with-sources s) #t))))

    (('insert sheet axis at)
     (let ((s (sheet-called kernel sheet)))
       (unless (if (eq? axis 'row)
                   (insert-row! s at)
                   (insert-column! s at))
         (throw 'cellar-kernel-error "there is no room to insert there"))
       (reply id (with-others kernel s (with-sources s) #t))))

    ;; Deleting takes the cells on the line with it, and leaves every reference
    ;; that named one saying so, so the sources go back here too.
    (('delete sheet axis at)
     (let ((s (sheet-called kernel sheet)))
       (unless (if (eq? axis 'row)
                   (delete-row! s at)
                   (delete-column! s at))
         (throw 'cellar-kernel-error
                "there is nothing to delete there, or it is the last one"))
       (reply id (with-others kernel s (with-sources s) #t))))

    (('recalculate sheet)
     (let ((s (sheet-called kernel sheet)))
       (invalidate-sheet! s)
       (reply id (with-others kernel s (snapshot-of s) #f))))

    (('snapshot sheet)
     (reply id (snapshot-of (sheet-called kernel sheet))))

    (('sources sheet)
     (let ((s (sheet-called kernel sheet)))
       (reply id `((sheet . ,(sheet-name s)) (sources . ,(sheet->alist s))))))

    (_ (throw 'cellar-kernel-error (format #f "no such request: ~a" op)))))

(define (reply id payload) (list 'reply id payload))

(define (sheet-called kernel sheet)
  (or (book-sheet kernel sheet)
      (throw 'cellar-kernel-error (format #f "no sheet called ~s is open" sheet))))

(define (sheet-name-given name)
  (if (string? name)
      name
      (throw 'cellar-kernel-error
             (format #f "a sheet is named with a string, not ~s" name))))

(define (reference name)
  (or (name->ref name)
      (throw 'cellar-kernel-error (format #f "not a cell: ~s" name))))


;;;
;;; What a sheet looks like from the other side
;;;

(define (snapshot-of s)
  "A sheet as the shell needs it: how big it is, and every cell that has
anything to show, already rendered.

Rendered, not raw.  The shell draws strings and paints backgrounds; it has no
evaluator and wants none, so what crosses the wire is what goes on the screen.
That is also what makes the grid's paint path free of any talking at all."
  `((sheet . ,(sheet-name s))
    (rows . ,(sheet-rows s))
    (columns . ,(sheet-columns s))
    (cells . ,(rendered-cells s))))

(define (with-sources s)
  (append (snapshot-of s) `((sources . ,(sheet->alist s)))))

(define (with-others kernel s payload sources?)
  "PAYLOAD, followed by every other sheet in the book.

Sheets name each other, so an edit to one is an answer about all of them: a
number on Summary changes the moment the cell it reads changes, and the shell
has no evaluator to work that out for itself.  Whether the others carry their
sources as well depends on whether anything rewrote them -- a move does, an
edit does not.

The whole book on every edit is more than the one sheet that was asked about,
and it is what the shell would otherwise have to ask for.  A dependency graph
would send less; a book is a handful of sheets of a few hundred cells, so it
would also be a great deal of machinery to save a message nobody is waiting
on."
  (append payload
          `((others . ,(map (lambda (other)
                              (if sources? (with-sources other)
                                  (snapshot-of other)))
                            (filter (lambda (other) (not (eq? other s)))
                                    (map (lambda (name) (book-sheet kernel name))
                                         (book-sheet-names kernel))))))))

(define (rendered-cells s)
  "Every cell that holds something, as

    (name display number? colour background error)

where ERROR is the message when the cell is one, and #f when it is not.

Everything the grid paints with and nothing else.  NUMBER? is here because a
sheet right-aligns its numbers and left-aligns everything else, and that is the
last thing the shell would otherwise need a value for; sending the answer costs
a boolean and saves the shell from ever holding a value at all.  The error
message travels because it is the cell's tooltip -- the grid shows `#ERR' and
says why on hover, and neither of those is something it should have to work out.

Cells that hold nothing are left out rather than sent as blanks.  A sheet is
mostly empty -- that is what a spreadsheet is -- and a hundred filled cells is
a small message where two and a half thousand mostly-empty ones would not be."
  (map (lambda (r)
         (let ((style (cell-style s r))
               (value (cell-value s r)))
           (list (ref->name r)
                 (cell-display s r)
                 (and (number? value) #t)
                 (and style (car style))
                 (and style (cdr style))
                 (and (cell-error? value) (cell-error-message value)))))
       (sheet-refs s)))


;;;
;;; The loop
;;;

(define %orphan-check-seconds 1)

(define (watch-for-orphanhood! parent)
  "Arrange to die if the shell does.

Ordinarily the kernel notices by reading end-of-file on its pipe, which is what
happens when the shell exits and its end is closed.  But the one situation this
process exists to survive is the one where it is spinning on a cell that will
not finish -- and a spinning kernel never gets back to its pipe to notice
anything at all.  Killed shell, orphaned kernel, a core at 100% until somebody
finds it.

So the check is hung on an alarm instead.  Guile runs a signal handler at the
next safe point in the running code, and a loop has one on every iteration, so
this fires even in the middle of the cell that is the problem.

Being orphaned is `getppid' no longer answering with the process that started
us.  It is tempting to test for 1 instead, and wrong: on a system with user
subreapers -- which is to say on a systemd session, which is to say on most of
them -- an orphan is adopted by the subreaper rather than by init, and a kernel
watching for 1 would wait for a parent it will never be given."
  (sigaction SIGALRM
    (lambda (signal)
      (unless (= (getppid) parent) (primitive-exit 0))
      (alarm %orphan-check-seconds)))
  (alarm %orphan-check-seconds))

(define (run-kernel input output)
  "Answer requests on INPUT with replies on OUTPUT until the other side goes
away.  Beyond the alarm above, that is the whole program: nothing runs in the
background, so a kernel that is not being asked something is a kernel doing
nothing at all."
  (watch-for-orphanhood! (getppid))
  (let ((kernel (make-kernel)))
    (let loop ()
      (let ((message (read-message input)))
        (unless (eof-object? message)
          (write-message output (kernel-serve kernel message))
          (loop))))))
