#!/usr/bin/env sbcl --script

(format t "args: ~a~%" sb-ext:*posix-argv*)

(load #p"main.lisp")
(sb-ext:save-lisp-and-die "main"
  :toplevel #'main  ; Set entry point
  :executable t)    ; Create a standalone executable
