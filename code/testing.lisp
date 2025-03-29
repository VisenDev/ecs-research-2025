(eval-when (:compile-toplevel :load-toplevel :execute) 
  (declaim (optimize (debug 3) (safety 3)))

  ;;;; ===========ANSI IO===========
  (defun ansi-esc (escape-code)
    (concatenate 'string (string (name-char "esc")) escape-code))

  (defun concat-symbols (&rest symbols)
    (intern (apply 'concatenate 'string (mapcar #'symbol-name symbols))))

  (defconstant +ansi-reset+   (ansi-esc "[0m"))

  (defmacro def-ansi-color (name escape-sequence)
    (let*
      ((constant-name (concat-symbols '+ansi- name '+))
       (function-name (concat-symbols 'print- name))
       )
      `(progn
         (defconstant ,constant-name (ansi-esc ,escape-sequence))
         (defun ,function-name (fmt &rest args)
           (format t "~a" ,constant-name)
           (apply 'format t fmt args)
           (format t "~a" +ansi-reset+))
         )
      )
    )

  (def-ansi-color red     "[31m")
  (def-ansi-color green   "[32m")
  (def-ansi-color yellow  "[33m")
  (def-ansi-color blue    "[34m")
  (def-ansi-color magenta "[35m")
  (def-ansi-color cyan    "[36m")

  (defun print-header (string)
    (let*
      ((min-width 40)
       (len (length string))
       (required-pad (- min-width len))
       (left-pad (floor (/ required-pad 2)))
       (right-pad (floor (/ (1+ required-pad) 2)))
       )
      (dotimes (_ left-pad) (format t "="))
      (print-yellow string)
      (dotimes (_ right-pad) (format t "="))
      (format t "~%")
    ))

  ;;;; ===========TESTING===========
  (defvar *tests* '())

  (defclass test-result ()
    ((name :accessor name :initarg :name)
     (successes :accessor successes :initform 0)
     (failures :accessor failures :initform 0)
     ))

  (defmacro deftest (name &body body)
    (format t "Defining new test named ~a~%" name)
    (push name *tests*)
    (let ((results (gensym)))
      `(defun ,name () 
         (let
           ((,results (make-instance 'test-result :name (quote ,name)))
            )
           (flet
             ((testing-expect (condition)
                              (if condition
                                (incf (successes ,results))
                                (incf (failures ,results))))
              )
             ,@body
             ,results ;return results for this test
             )
           )
         )
      )
    )

  (defun run-tests ()
    (print-header "RUNNING TESTS")
    (let*
      ((total-successes 0)
       (total-failures 0)
       )
      (dolist (test *tests*)
        (print-magenta "  > ~36a" test)
        (let*
          ((result (funcall test))
           (s (successes result))
           (f (failures result))
           )
          (incf total-successes s)
          (incf total-failures f)
          (if (> f 0)
            (print-red   "[FAIL]    ~a/~a~%" f (+ f s))
            (print-green "[SUCCESS] ~a/~a~%" s (+ f s))
            )
          )
        )
      (format t "~%")
      (print-header "RESULTS")
      (print-green "  > [TOTAL SUCCESSES] ~a~%" total-successes)
      (print-red   "  > [TOTAL  FAILURES] ~a~%" total-failures)
      )
    )
  )

