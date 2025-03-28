(eval-when (:compile-toplevel :load-toplevel :execute) 

  (defvar *tests* '())
  (defvar *test-results* '())
  (declaim (optimize (debug 3) (safety 3)))

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

  ;  (defun print-test-result ()
  ;    (let*
  ;      ((total-successes 0)
  ;       (total-failures 0)
  ;       )
  ;      (dolist (result *test-results*)
  ;        (let ((s (successes result))
  ;              (f (failures result))
  ;              (name (name result)))
  ;          (incf total-successes s)
  ;          (incf total-failures f)
  ;          (if (> f 0)
  ;            (format t "Test ~a failed with ~a out of ~a failed conditions~%"
  ;                    name f (+ f s))
  ;            (format t "Test ~a succeeded with ~a out of ~a succeeded conditions~%"
  ;                    name s (+ f s))
  ;            )
  ;          )
  ;        )
  ;      (format t "~%")
  ;      (format t "TOTAL SUCCESSES: ~a~%" total-successes)
  ;      (format t "TOTAL FAILURES ~a~%" total-failures)
  ;      )
  ;    )

  (defun run-tests ()
    (format t "RUNNING TESTS...~%~%")
    (let*
      ((total-successes 0)
       (total-failures 0)
       )
      (dolist (test *tests*)
        (format t "Starting test ~20a...   " test)
        (let*
          ((result (funcall test))
           (s (successes result))
           (f (failures result))
           )
          (incf total-successes s)
          (incf total-failures f)
          (if (> f 0)
            (format t "   failed with ~a out of ~a failed conditions~%"
                    f (+ f s))
            (format t "   succeeded with ~a out of ~a succeeded conditions~%"
                    s (+ f s))
            )
          )
        )
      (format t "~%")
      (format t "TOTAL SUCCESSES: ~a~%" total-successes)
      (format t "TOTAL FAILURES ~a~%" total-failures)
      )
    )

  )

