;;; The preference store and the command parsing behind the external editor.
;;; Both are free of GTK, so this needs no display.

(use-modules (cellar config))

(define failures 0)
(define (check label expected actual)
  (if (equal? expected actual)
      (format #t "  ok   ~a~%" label)
      (begin (set! failures (+ failures 1))
             (format #t "  FAIL ~a: expected ~s got ~s~%" label expected actual))))

(format #t "-- splitting a command~%")
(check "a bare word" '("vim") (split-command "vim"))
(check "words" '("emacsclient" "-c") (split-command "emacsclient -c"))
(check "runs of spaces" '("a" "b") (split-command "  a   b  "))
(check "double quotes group"
       '("/opt/My Editor/run" "-w")
       (split-command "\"/opt/My Editor/run\" -w"))
(check "single quotes are literal"
       '("say \"hi\"") (split-command "'say \"hi\"'"))
(check "backslash escapes a space"
       '("/opt/My Editor") (split-command "/opt/My\\ Editor"))
(check "nothing at all" '() (split-command "   "))

(format #t "-- building the argument vector~%")
(check "the file is added at the end"
       '("gnome-text-editor" "/tmp/A1.scm")
       (editor-argv "gnome-text-editor" "/tmp/A1.scm"))
(check "%s says where the file goes"
       '("xterm" "-e" "vim" "/tmp/A1.scm")
       (editor-argv "xterm -e vim %s" "/tmp/A1.scm"))
(check "%s inside an argument"
       '("code" "--goto" "/tmp/A1.scm:1")
       (editor-argv "code --goto %s:1" "/tmp/A1.scm"))
(check "%% is a literal per cent"
       '("weird" "100%" "/tmp/A1.scm")
       (editor-argv "weird 100%%" "/tmp/A1.scm"))
(check "a path with a space stays one argument"
       '("vim" "/tmp/my sheets/A1.scm")
       (editor-argv "vim %s" "/tmp/my sheets/A1.scm"))
(check "an empty command is an empty vector" '() (editor-argv "  " "/tmp/A1.scm"))

(format #t "-- which editor a cell opens in~%")
(define config-file
  (string-append (or (getenv "TMPDIR") "/tmp") "/cellar-config-test.scm"))
(setenv "CELLAR_CONFIG" config-file)
(when (file-exists? config-file) (delete-file config-file))
(unsetenv "CELLAR_EDITOR")

(load-config!)
(check "the built-in editor is the default" #f (effective-editor-command))
(check "and there is no command yet" "" (external-editor-command))

(set-external-editor-command! "gnome-text-editor")
(check "a command alone does not switch editors" #f (effective-editor-command))
(set-external-editor-enabled! #t)
(check "the switch is what does" "gnome-text-editor" (effective-editor-command))
(set-external-editor-command! "   ")
(check "a blank command falls back to the built-in editor"
       #f (effective-editor-command))
(set-external-editor-command! "gnome-text-editor")

(format #t "-- preferences survive a restart~%")
(save-config!)
(set-external-editor-enabled! #f)
(set-external-editor-command! "forgotten")
(load-config!)
(check "the switch came back" #t (external-editor-enabled?))
(check "so did the command" "gnome-text-editor" (external-editor-command))

(format #t "-- a config file we cannot make sense of~%")
(call-with-output-file config-file (lambda (port) (display "not a sheet(" port)))
(load-config!)
(check "leaves the defaults standing" #f (external-editor-enabled?))
(check "and no command" "" (external-editor-command))
(call-with-output-file config-file
  (lambda (port) (write '((external-editor-enabled . #t) (nonsense . 42)) port)))
(load-config!)
(check "keys we know are read" #t (external-editor-enabled?))
(check "keys we do not are dropped" "" (external-editor-command))

(format #t "-- CELLAR_EDITOR overrides the preference~%")
(setenv "CELLAR_EDITOR" "vim %s")
(set-external-editor-enabled! #f)
(check "even with the switch off" "vim %s" (effective-editor-command))
(check "and it says so" "vim %s" (editor-override))
(setenv "CELLAR_EDITOR" "")
(set-external-editor-enabled! #t)
(set-external-editor-command! "gnome-text-editor")
(check "empty forces the built-in editor" #f (effective-editor-command))
(check "which is reported as 'internal" 'internal (editor-override))
(unsetenv "CELLAR_EDITOR")
(check "unset means the preference decides" #f (editor-override))

(when (file-exists? config-file) (delete-file config-file))

(format #t "~%~a~%" (if (zero? failures) "ALL TESTS PASSED" (format #f "~a FAILURE(S)" failures)))
(exit (if (zero? failures) 0 1))
