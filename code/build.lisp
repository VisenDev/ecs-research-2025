#!/usr/bin/env sbcl --script

(declaim (optimize (debug 3) (safety 3)))

(unless (find-package :asdf)
  (load #p"asdf/asdf.lisp"))

(defun parse-argv ()
  (let ((arg (ignore-errors (second sb-ext:*posix-argv*))))
    (if arg
      (intern (string-upcase arg))
      'all)))

(defun build (arg)
  (handler-case 
    (ccase
      arg

      ;;;; THE BUILD OPTIONS
      (all
        (build 'test))

      (load
        ;(load #p"main.lisp")
        ;(asdf:load-asd #p"./ecs-research-2025.asd")
        ;(print asdf:*source-registry-parameter*)
        (asdf:initialize-source-registry '(:source-registry (:directory :here) :inherit-configuration))
        ;(print asdf:*source-registry-parameter*)
        (asdf:load-system 'ecs-research-2025)
        ;(print asdf:*source-registry-parameter*)
        )

      (exe
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
    (type-error (e) (format t "Invalid command line argument~%~a" e)))
  (quit)
  )

(build (parse-argv))




;(defmacro include (path)
;  (format t "including ~a~%" path)
;  `(progn
;     ,@(print (with-open-file (fp path)
;         (loop :for sexpr = (read fp nil nil)
;               :while sexpr
;               :collect sexpr)
;         ))
;     )
;  )
