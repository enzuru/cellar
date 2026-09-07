;;; Cellar -- tests for the protocol and for the kernel behind it.
;;;
;;; The protocol is exercised in this process; the kernel is exercised as a real
;;; subprocess over a real pipe, because a pipe is what it will be talking over
;;; and because half of what is being tested is that it says nothing on stdout
;;; that is not a reply.  Nothing here needs a display.
;;;
;;; This is the kernel's own side only.  The half that reads these messages is
;;; the shell, which is Haskell; its end of the same conversation is tested in
;;; test/Spec.hs, against a kernel started the same way.

(use-modules (cellar protocol)
             (ice-9 popen)
             (ice-9 match)
             (rnrs bytevectors))

(define failures 0)
(define (check label expected actual)
  (if (equal? expected actual)
      (format #t "  ok   ~a~%" label)
      (begin (set! failures (+ failures 1))
             (format #t "  FAIL ~a: expected ~s got ~s~%" label expected actual))))


;;;
;;; The framing
;;;

(format #t "-- framing~%")

(define (join . bytevectors)
  (let* ((total (apply + (map bytevector-length bytevectors)))
         (whole (make-bytevector total)))
    (let loop ((rest bytevectors) (at 0))
      (if (null? rest)
          whole
          (begin
            (bytevector-copy! (car rest) 0 whole at
                              (bytevector-length (car rest)))
            (loop (cdr rest) (+ at (bytevector-length (car rest)))))))))

(define %messages
  '((request 1 set-cell "Summary" "D6" "(sum (range 'D2 'D4))")
    (reply 1 ((sheet . "Summary") (rows . 100) (columns . 26)
              (cells ("A1" "Qty" #f #f #f) ("D6" "#ERR" #f #f #t))))
    (fail 2 "no sheet called \"Q1\" is open")
    ;; A cell holding the punctuation the wire is made of.
    (request 3 set-cell "S" "A1" "a ) and a \" and a \\ walk into a bar")))

(let* ((blob (apply join (map message->bytevector %messages)))
       (decoder (make-decoder)))
  ;; A byte at a time, which is the worst a pipe can do to the shell.
  (let ((got (let loop ((i 0) (acc '()))
               (if (= i (bytevector-length blob))
                   acc
                   (begin
                     (decoder-feed! decoder
                                    (u8-list->bytevector
                                     (list (bytevector-u8-ref blob i))))
                     (loop (+ i 1) (append acc (decoder-take! decoder))))))))
    (check "a message survives being delivered a byte at a time"
           %messages got))
  (check "and the decoder is left holding nothing" '()
         (decoder-take! decoder)))

(let ((decoder (make-decoder)))
  (decoder-feed! decoder (apply join (map message->bytevector %messages)))
  (check "or all at once" %messages (decoder-take! decoder)))

(let* ((decoder (make-decoder))
       (whole (message->bytevector '(request 2 ping)))
       (cut (- (bytevector-length whole) 3)))
  (decoder-feed! decoder (let ((head (make-bytevector cut)))
                           (bytevector-copy! whole 0 head 0 cut)
                           head))
  (check "a half-arrived message is not a message yet" '()
         (decoder-take! decoder))
  (decoder-feed! decoder (let ((tail (make-bytevector 3)))
                           (bytevector-copy! whole cut tail 0 3)
                           tail))
  (check "and completes when the rest turns up"
         '((request 2 ping))
         (decoder-take! decoder)))


;;;
;;; The kernel, over a pipe
;;;

(format #t "-- the kernel over a pipe~%")

(define kernel
  (open-input-output-pipe
   "GUILE_AUTO_COMPILE=0 guile -L src -s bin/cellar-kernel.scm"))

(define *id* 0)

(define (ask op . arguments)
  "Send a request and wait for its reply.  A test may block; the shell may not."
  (set! *id* (+ *id* 1))
  (write-message kernel (cons* 'request *id* op arguments))
  (let ((message (read-message kernel)))
    (match message
      (('reply id payload) payload)
      (('fail id message) (list 'failed message))
      (_ (list 'broken message)))))

(define (payload key answer)
  (and (list? answer) (assq-ref answer key)))

(check "it answers a ping" '() (ask 'ping))

(ask 'open "S" 12 4 '(("A1" . "\"Qty\"") ("A2" . "7") ("B2" . "(* A2 6)")))

(check "a sheet opens at exactly the size it was given" 12
       (payload 'rows (ask 'snapshot "S")))
(check "in both directions -- how big is the shell's business, not the kernel's"
       4 (payload 'columns (ask 'snapshot "S")))

(check "cells come back rendered"
       '(("A1" "Qty" #f #f #f #f) ("A2" "7" #t #f #f #f) ("B2" "42" #t #f #f #f))
       (payload 'cells (ask 'snapshot "S")))

(check "setting a cell recomputes what depends on it"
       '(("A1" "Qty" #f #f #f #f) ("A2" "10" #t #f #f #f) ("B2" "60" #t #f #f #f))
       (payload 'cells (ask 'set-cell "S" "A2" "10")))

(check "and echoes the source as the model kept it" "10"
       (payload 'source (ask 'set-cell "S" "A2" "  10  ")))

(check "an emptied cell has no source at all" #f
       (payload 'source (ask 'set-cell "S" "A1" "   ")))

(check "an error is rendered as one, and says why"
       '("#ERR" #f #f #f #t)
       (let* ((cells (payload 'cells (ask 'set-cell "S" "A2" "(/ 1 0)")))
              (cell (assoc "A2" cells)))
         (match cell
           ((name display number? colour background error)
            ;; The message itself is Guile's and not ours to pin down; that
            ;; there is one, and that it is a string, is the contract.
            (list display number? colour background (string? error)))
           (_ cell))))

(check "a colour travels with the value"
       '("A3" "red" #f #f "#fff3b0" #f)
       (let ((cells (payload 'cells
                             (ask 'set-cell "S" "A3"
                                  "(styled \"red\" #:background \"#fff3b0\")"))))
         (assoc "A3" cells)))

(check "a preview is evaluated without being kept" "9"
       (payload 'display (ask 'preview "S" "A4" "(* 3 3)")))
(check "so the cell it previewed is still empty" #f
       (assoc "A4" (payload 'cells (ask 'snapshot "S"))))

(format #t "-- rearranging~%")
(ask 'open "R" 5 3 '(("A1" . "1") ("A2" . "2") ("B1" . "(+ A1 A2)")))
(let ((answer (ask 'move "R" 'row 0 1)))
  (check "a move reports the sources it rewrote"
         '(("A1" . "2") ("A2" . "1") ("B2" . "(+ A2 A1)"))
         (payload 'sources answer))
  (check "and the sum is unchanged by the move"
         "3"
         (cadr (assoc "B2" (payload 'cells answer)))))

(let ((answer (ask 'insert "R" 'row 0)))
  (check "an insert grows the sheet" 6 (payload 'rows answer))
  (check "and pushes the references down"
         '(("A2" . "2") ("A3" . "1") ("B3" . "(+ A3 A2)"))
         (payload 'sources answer)))

(format #t "-- sheets that name each other~%")

(define (other-named name answer)
  "The snapshot of the sheet called NAME that came along with ANSWER."
  (let loop ((others (payload 'others answer)))
    (cond ((not (pair? others)) #f)
          ((equal? (assq-ref (car others) 'sheet) name) (car others))
          (else (loop (cdr others))))))

(ask 'open "Books" 6 3 '(("A1" . "20") ("A2" . "22")))
(let ((answer (ask 'open "Ledger" 6 3
                   '(("A1" . "(+ Books!A1 Books!A2)")
                     ("A2" . "(cell \"Books\" 'A1)")))))
  (check "a cell reads a cell on another sheet"
         '("A1" "42" #t #f #f #f)
         (assoc "A1" (payload 'cells answer)))
  (check "and the sheets it was opened beside come back with it"
         "Books"
         (and (other-named "Books" answer) "Books")))

(let ((answer (ask 'set-cell "Books" "A1" "100")))
  (check "an edit to one sheet is answered for the others"
         '("A1" "122" #t #f #f #f)
         (assoc "A1" (payload 'cells (other-named "Ledger" answer))))
  (check "which do not carry sources, because nothing rewrote them"
         #f
         (assq-ref (other-named "Ledger" answer) 'sources)))

(let ((answer (ask 'move "Books" 'row 0 1)))
  (check "a move rewrites the references on other sheets"
         '(("A1" . "(+ Books!A2 Books!A1)") ("A2" . "(cell \"Books\" 'A2)"))
         (assq-ref (other-named "Ledger" answer) 'sources))
  (check "and leaves them saying what they said"
         '("A1" "122" #t #f #f #f)
         (assoc "A1" (payload 'cells (other-named "Ledger" answer)))))

(let ((answer (ask 'rename "Books" "Old Books")))
  (check "a rename says so wherever the sheet is named"
         '(("A1" . "(+ #{Old Books!A2}# #{Old Books!A1}#)")
           ("A2" . "(cell \"Old Books\" 'A2)"))
         (assq-ref (other-named "Ledger" answer) 'sources))
  (check "and the renamed sheet answers to its new name" 6
         (payload 'rows (ask 'snapshot "Old Books"))))

(let ((answer (ask 'close "Old Books")))
  (check "closing a sheet leaves what read it in error"
         "#ERR"
         (cadr (assoc "A1" (payload 'cells (other-named "Ledger" answer))))))

(format #t "-- refusing what it cannot do~%")
(check "a sheet that is not open"
       '(failed "no sheet called \"nope\" is open")
       (ask 'snapshot "nope"))
(check "a request it does not know"
       '(failed "no such request: fly")
       (ask 'fly "R"))
(check "something that is not a cell"
       '(failed "not a cell: \"zzz\"")
       (ask 'set-cell "R" "zzz" "1"))
(check "a move that goes nowhere"
       '(failed "that line is already at the edge")
       (ask 'move "R" 'row 0 0))
(check "a sheet named with something that is not a string"
       '(failed "a sheet is named with a string, not 7")
       (ask 'open 7 4 4 '()))
(check "a rename onto a name already in use"
       '(failed "a sheet called \"R\" is already open")
       (ask 'rename "Ledger" "R"))
(check "and it is still answering afterwards" '() (ask 'ping))

(format #t "-- closing~%")
(ask 'close "S")
(check "a closed sheet is gone"
       '(failed "no sheet called \"S\" is open")
       (ask 'snapshot "S"))

(close-pipe kernel)


(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED"
                        (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
