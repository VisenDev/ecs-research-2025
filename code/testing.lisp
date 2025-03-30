  (declaim (optimize (debug 3) (safety 3)))

  ;;;; ===========ANSI IO===========
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute) 
      ,@body
      ))

  (eval-always
    (defmacro ansi-esc (escape-code)
      `(concatenate 'string (string (name-char "esc")) ,escape-code))

    (defun concat-symbols (&rest symbols)
      (intern (apply 'concatenate 'string (mapcar #'symbol-name symbols))))
  )

  (defparameter +ansi-reset+   (ansi-esc "[0m"))

  (defmacro def-ansi-color (name escape-sequence)
    (let*
      ((constant-name (concat-symbols '+ansi- name '+))
       (function-name (concat-symbols 'print- name))
       )
      `(progn
         (defparameter ,constant-name (ansi-esc ,escape-sequence))
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
      ((min-width 50)
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

  (defclass test-case ()
    ((result :accessor result :initarg :result :initform (error "mandatory initialization"))
     (expected :accessor expected :initarg :expected :initform t)

     (form :accessor form :initarg :form)
     (file :accessor file :initarg :file)
     (line :accessor line :initarg :line)
     ))

  (defun print-test-case (case)
    (if (equalp (result case) (expected case))
      (progn
        (incf *total-pass*)
        )
      (progn
        (incf *total-fail*)
        (format t "    Test case ~a failed, expected ~a, got ~a~%"
                (form case) (expected case) (result case))
        (unless (and (null (line case)) (file case))
          (format t "        See ~a line ~a~%" (file case) (line case))
          )
        )
      )
    case
    )

  (defmacro define-test-case (form expected)
    `(print-test-case
      (make-instance
         'test-case
         :form (quote ,form)
         :result ,form
         :expected ,expected
         :file SB-EXT:COMPILE-FILE-LINE
         :line SB-EXT:COMPILE-FILE-POSITION
         ))
    )

  (defvar *test-results*'())
  (defvar *tests*'())
  (defvar *total-pass* 0)
  (defvar *total-fail* 0)

  (defmacro testing-expect (form)
    `(push (define-test-case ,form t) *test-results*))

  (defmacro testing-expect-equal (form expected)
    `(push (define-test-case ,form ,expected) *test-results*))

  (defmacro deftest (name &body body)
    (push name *tests*)
    `(defun ,name ()
       ,@body
       ))
    

  (defun run-tests ()
    (print-header "RUNNING TESTS")
    (dolist (test *tests*)
      (print-magenta "  > ~33a" test)
      (funcall test)
      ))



;  (defvar *tests* '())

;  (defclass failed-form ()
;    ((form :accessor form :initarg form)
;     (file :accessor file :initarg file :initform "<unknown file>")
;     (line :accessor line :iniarg line "<unknown line>")
;     ))
;
;  (defun make-failed-form (form env)
;    (declare (ignore env))
;    (make-instance 'failed-form :form form))
;
;
;  (defclass test-result ()
;    ((name :accessor name :initarg :name)
;     (successes :accessor successes :initform 0)
;     (failures :accessor failures :initform 0)
;     (failed-forms :accessor failed-forms :initform '())
;     ))
;
;  (defun make-test-result (name)
;    (make-instance 'test-result :name name))
;
;  (defmacro testing-expect (form &environment env)
;    `(testing-expect-internal ,form (quote ,form) ,env)
;    )
;
;  (defun testing-expect-internal (form quoted-form env)
;    (let*
;      ((results (make-test-result name))
;       )
;      (
;
;
;
;
;  (defmacro deftest (name &body body)
;    (format t "Defining new test named ~a~%" name)
;    (push name *tests*)
;    (let ((results (gensym)) (failed-forms (gensym)))
;      `(defun ,name () 
;         (let
;           ((,results (make-instance 'test-result :name (quote ,name)))
;            (,failed-forms '())
;            )
;           (flet
;             ((testing-expect-internal
;                (condition form env)
;                (if condition
;                  (incf (successes ,results))
;                  (progn 
;                    (incf (failures ,results))
;                    (push (list form env) ,failed-forms)
;                    )
;                  ))
;              )
;               ,@body
;               )
;             )
;           )
;         )
;      )
;    )
;
;  (defun print-test-result (result-list)
;    (multiple-value-bind (file line column) (sb-debug:form-source-location env)
;
;  (defun run-tests ()
;    (format t "~%")
;    (print-header "RUNNING TESTS")
;    (let*
;      ((total-successes 0)
;       (total-failures 0)
;       )
;      (dolist (test *tests*)
;        (print-magenta "  > ~33a" test)
;        (let*
;          ((result-list (funcall test))
;           (result (first result-list))
;           (failed-forms (second result-list))
;           (s (successes result))
;           (f (failures result))
;           )
;          (incf total-successes s)
;          (incf total-failures f)
;          (if (> f 0)
;            (progn
;              (print-red   "[FAIL]    ~a/~a~%" s (+ f s))
;              (format t "Failing test cases: ~%~a~%" failed-forms)
;              )
;            (print-green "[SUCCESS] ~a/~a~%" s (+ f s))
;            )
;          )
;        )
;      (format t "~%")
;      (print-header "RESULTS")
;      (print-green "  > [TOTAL SUCCESSES] ~a~%" total-successes)
;      (print-red   "  > [TOTAL  FAILURES] ~a~%" total-failures)
;      )
;    )
;  )
;;
