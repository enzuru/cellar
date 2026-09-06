;;; Cellar -- what the two halves of the program say to each other.
;;;
;;; Cellar runs as two processes.  The kernel owns the sheets and evaluates
;;; cells; the shell owns the window, the folder on disk and the tabs.  They
;;; talk over a pipe, in s-expressions, because the thing being talked about is
;;; Scheme source text and there is no sense translating it into something else
;;; on the way past.
;;;
;;; A message is its byte count, a newline, and then that many bytes of UTF-8:
;;;
;;;     47
;;;     (request 12 set-cell "Summary" "D6" "(* 6 7)")
;;;
;;; The count is what makes the shell able to read without ever blocking.  A
;;; bare `read' on a pipe blocks until it has a whole datum, and a UI that does
;;; that has handed its responsiveness to the other process -- which is the one
;;; thing this split exists to prevent.  With a count in front, the shell can
;;; look at what has arrived, decide whether a whole message is there, and go
;;; back to drawing if it is not.  The kernel has nothing else to do and reads
;;; the simple blocking way.
;;;
;;; The vocabulary is deliberately small and flat, and it is a contract rather
;;; than an accident: something other than Guile is expected to speak this one
;;; day, and every message is a list whose head is a symbol, whose arguments
;;; are strings, integers, symbols and lists of those, and whose payload is an
;;; alist.  Nothing here relies on the reader being Guile's.
;;;
;;;     shell -> kernel   (request <id> <op> <argument> ...)
;;;     kernel -> shell   (reply <id> <alist>)
;;;                       (fail <id> "what went wrong")
;;;
;;; Cell sources travel as strings, so a cell holding a close paren or a quote
;;; is a string with a close paren in it and nothing has to be escaped twice.

(define-module (cellar protocol)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 textual-ports)
  #:export (write-message
            read-message
            make-decoder
            decoder-feed!
            decoder-take!
            message->bytevector))

(define (message->bytevector datum)
  "DATUM as the bytes that go down the pipe, count and all."
  (let* ((text (call-with-output-string (lambda (port) (write datum port))))
         (payload (string->utf8 text))
         (header (string->utf8
                  (string-append (number->string (bytevector-length payload))
                                 "\n"))))
    (let ((whole (make-bytevector (+ (bytevector-length header)
                                     (bytevector-length payload)))))
      (bytevector-copy! header 0 whole 0 (bytevector-length header))
      (bytevector-copy! payload 0 whole (bytevector-length header)
                        (bytevector-length payload))
      whole)))

(define (write-message port datum)
  "Write DATUM to PORT and push it out.  The flush is the whole point: the
other side is waiting on it, and a message sitting in a buffer is a message
that has not been sent."
  (put-bytevector port (message->bytevector datum))
  (force-output port))


;;;
;;; Reading, the blocking way -- the kernel's
;;;

(define (read-message port)
  "The next message on PORT, or the end-of-file object when the other side has
gone.  Blocks, which is right for the kernel: waiting to be asked something is
all it has to do."
  (let ((count (read-count port)))
    (if (eof-object? count)
        count
        (let ((payload (get-bytevector-n port count)))
          (if (or (eof-object? payload)
                  (< (bytevector-length payload) count))
              (eof-object)
              ;; A payload that is not a readable datum is news, not a reason to
              ;; fall over.  The kernel holds sheets that only exist in memory
              ;; until they are written, and dying over one bad frame would
              ;; take them with it.
              (catch #t
                (lambda () (call-with-input-string (utf8->string payload) read))
                (lambda arguments (list 'unreadable))))))))

(define (read-count port)
  "The byte count at the head of a message.  Reads one byte at a time up to the
newline, so that not one byte of the payload is swallowed with it."
  (let loop ((digits '()))
    (let ((byte (get-u8 port)))
      (cond
       ((eof-object? byte)
        (if (null? digits) byte (count-of digits)))
       ((eqv? byte 10) (count-of digits))
       (else (loop (cons byte digits)))))))

(define (count-of digits)
  (let ((text (utf8->string (u8-list->bytevector (reverse digits)))))
    (or (string->number (string-trim-both text))
        (throw 'cellar-protocol-error
               (format #f "a message whose length is ~s" text)))))


;;;
;;; Reading, the way that does not block -- the shell's
;;;

;; A decoder is a bag of bytes that have arrived and not yet made a whole
;; message.  The shell hands it whatever the pipe had this time round the main
;; loop and takes back the messages that completed, which may be none.

(define-record-type <decoder>
  (%make-decoder pending)
  decoder?
  (pending decoder-pending set-decoder-pending!))

(define (make-decoder)
  (%make-decoder (make-bytevector 0)))

(define (decoder-feed! decoder bytes)
  "Add BYTES, a bytevector of whatever just arrived, to what DECODER is holding."
  (let* ((old (decoder-pending decoder))
         (whole (make-bytevector (+ (bytevector-length old)
                                    (bytevector-length bytes)))))
    (bytevector-copy! old 0 whole 0 (bytevector-length old))
    (bytevector-copy! bytes 0 whole (bytevector-length old)
                      (bytevector-length bytes))
    (set-decoder-pending! decoder whole)))

(define (decoder-take! decoder)
  "Every whole message DECODER is now holding, in the order they arrived, taken
out of it.  Returns the empty list when nothing has completed yet, which is the
ordinary answer and not a problem."
  (let loop ((messages '()))
    (let ((message (take-one! decoder)))
      (if message
          (loop (cons message messages))
          (reverse messages)))))

(define (take-one! decoder)
  "The first whole message in DECODER, removed, or #f when there is not one."
  (let* ((pending (decoder-pending decoder))
         (newline (find-byte pending 10)))
    (and newline
         (let ((count (count-of
                       (reverse (bytevector->u8-list
                                 (slice pending 0 newline)))))
               (start (+ newline 1)))
           (and (>= (- (bytevector-length pending) start) count)
                (let ((payload (slice pending start (+ start count))))
                  (set-decoder-pending!
                   decoder
                   (slice pending (+ start count)
                          (bytevector-length pending)))
                  (call-with-input-string (utf8->string payload) read)))))))

(define (find-byte bytes wanted)
  (let ((length (bytevector-length bytes)))
    (let loop ((i 0))
      (cond ((= i length) #f)
            ((eqv? (bytevector-u8-ref bytes i) wanted) i)
            (else (loop (+ i 1)))))))

(define (slice bytes from to)
  (let ((piece (make-bytevector (- to from))))
    (bytevector-copy! bytes from piece 0 (- to from))
    piece))
