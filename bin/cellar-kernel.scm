;;; Cellar -- the kernel, as a program.
;;;
;;; Reads requests on stdin and writes replies on stdout, both framed as
;;; (cellar protocol) describes.  Nothing else may be written to stdout: it is
;;; the wire, and a stray `display' in here would be read by the shell as a
;;; malformed message.  Warnings and backtraces go to stderr, which the shell
;;; leaves alone.

(use-modules (cellar kernel))

(run-kernel (current-input-port) (current-output-port))
