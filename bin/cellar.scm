;;; Cellar launcher.
;;;
;;; A single-token entry point: passing `-e (@ (cellar main) main)' through a
;;; wrapper means fighting shell quoting, whereas `-s this-file' does not.

(use-modules (cellar main))

(main (command-line))
