#!/usr/bin/env sbcl --script

(defun parse-argv ()
  (let ((arg (ignore-errors (second sb-ext:*posix-argv*))))
    (if arg
      (intern (string-upcase arg))
      'all)))


(defun build (arg)
  (handler-case 
    (ecase arg

      ;;;; THE BUILD OPTIONS
      (all
        (build 'test))

      (load 
        (load #p"main.lisp"))

      (compile
        (build 'load)
        (sb-ext:save-lisp-and-die
          "main"
          :toplevel #'main; Set entry point
          :executable t)    ; Create a standalone executable
        )

      (test
        (build 'load)
        (run-tests)
        )
      )

    ;;;; PRINT ERROR IF INVALID ARG PROVIDED
    (t (e) (format t "~a" e)))
  )

(build (parse-argv))
