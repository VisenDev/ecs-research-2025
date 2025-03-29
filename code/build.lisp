#!/usr/bin/env sbcl --script

(defun parse-argv ()
  (let ((argv sb-ext:*posix-argv*))
    (if (> (length argv) 1)
      (intern (string-upcase (second argv)))
      'test)
    ))

(defun build (arg)
  (case arg
    (all
      (load #p"main.lisp")
      (sb-ext:save-lisp-and-die "main"
        :toplevel #'main; Set entry point
        :executable t)    ; Create a standalone executable
      )
    (test
      (load #p"main.lisp")
      (run-tests)
      )
    (t (format t "ERROR, expected 'all' or 'test'"))
    )
  )

(build (parse-argv))
(exit)
