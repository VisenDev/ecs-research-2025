;;;; ===================== Imports and settings ====================

(declaim (optimize (debug 3) (safety 3)))

;
;(eval-when (:compile-toplevel :load-toplevel :execute) 
;
;  (unless (find-package :asdf)
;    (load #p"asdf/asdf.lisp"))
;  (load #p"closer-mop/closer-mop.asd")
;
;  (load #p"macros.lisp")
;  (load #p"testing.lisp")
;  )


;;;;======================= Vector =======================

(defun make-vec (type)
  (make-array 0 :element-type type :fill-pointer t :adjustable t))

(defun vec-push (self val)
  (vector-push-extend val self))

(deftest
  test-vec
  (quick-properties 
    (let ((vec (make-vec 'integer)))
      (testing-expect (= vec.length -0))
      (vec-push vec 1)
      (testing-expect (= vec.length 1))
      (loop :for i :from 0 :below 10 :do
            (vec-push vec i))
      (testing-expect (= vec.length 11))
      )))

;;;; ======================= Sparse Set =======================

(defclass sset ()
  ((dense :initarg :dense :accessor dense)
   (sparse :initform (make-vec 'integer) :accessor sparse)
   (dense-to-sparse :initform (make-vec 'integer) :accessor dense-to-sparse)
   ))


(defmacro sset-accessors (instance &body body)
  `(with-accessors ((dense dense)
                    (sparse sparse)
                    (dense-to-sparse dense-to-sparse)
                    )
     ,instance
     ,@body)
  )

(defun make-sset (type)
  (make-instance 'sset :dense (make-vec type))
  )


(defun sset-get (self i)
  (quick-properties
    (orelse nil (aref self.dense (aref self.sparse i)))
    ))

(defun sset-len (self)
  (quick-properties self.dense.length)
  )

(defun sset-extend (self new-max-index)
  (quick-properties
    (when (< self.sparse.length new-max-index)
      (loop :for i :from self.sparse.length :to new-max-index :do
            (vec-push self.sparse -1))
      )))

(defun sset-set (self i val)
  (quick-properties
    (if (null (sset-get self i))
      (let*
        ((new-dense-index (sset-len self))
         )
        (vec-push self.dense val)
        (vec-push self.dense-to-sparse i)
        (sset-extend self i)
        (setf (aref self.sparse i) new-dense-index)
        )
      (let*
        ((dense-index (aref self.sparse i))
         )
        (setf (aref (dense self) dense-index) val)
        )
      )))

;(defun sset-remove (self i)
;  (when (null (sset-get self i))
;    (return-from sset-remove))
;  (with-accessors self ()
;  (let*
;      ((dense-index (aref (sparse self) i))
;       (top-i (aref (
;       )
;    (setf (aref (sparse self) i) -1)
;    (


(deftest
  test-sset
  (let
    ((sset (make-sset 'symbol)))
      (testing-expect (= (sset-len sset) 0))

      (sset-set sset 10 'foo)
      (testing-expect (= (sset-len sset) 1))
      (testing-expect (eq (sset-get sset 10) 'foo))

      (sset-set sset 10 'bar)
      (testing-expect (= (sset-len sset) 1))
      (testing-expect (eq (sset-get sset 10) 'bar))

      (sset-set sset 100 'var)
      (testing-expect (= (sset-len sset) 2))
      (testing-expect (eq (sset-get sset 10) 'bar))
      (testing-expect (eq (sset-get sset 100) 'var))
      (testing-expect (eq (sset-get sset 100) 'vnr))
    )
  )






;;;; ======================= ECS =======================

(defclass ecs ()
  ((component-types :initform (make-hash-table) :accessor component-types)
   (components :initform (make-hash-table) :accessor components)
   (ids :initform '() :accessor ids)
   (highest-id :initform 0 :accessor highest-id)
   ))

(defun get-type-of-component (ecs component-symbol)
  (quick-properties
    (gethash component-symbol ecs.component-types)
    ))

(defun define-component (ecs component-symbol component-type)
  (quick-properties
    ;(unless (null (get-type-of-component ecs component-symbol))
    ;  (error "component ~a already defined as having type ~a"
    ;         component-symbol
    ;         (get-type-of-component ecs component-symbol)
    ;         ))
    (setf (gethash component-symbol ecs.component-types ecs) component-type)
    (setf
      (gethash component-symbol ecs.components) 
      (make-array 16 :element-type component-type :fill-pointer t :adjustable t)
      )
    ))

(defun set-component (ecs component-symbol value)
  (unless (equalp (get-type-of-component ecs component-symbol) (type-of value))
    (error "component ~a expects type ~a but was given value of type ~a"
           component-symbol
           (get-type-of-component ecs component-symbol)
           (type-of value)
           )
    )
  )


(defparameter *ecs* (make-instance 'ecs))
(defun main()
  (format t "~a~%" *ecs*)
  (define-component *ecs* 'name 'integer)
  (define-component *ecs* 'id 'integer)
  (format t "~a~%" *ecs*)
  )
;(set-componet *ecs* 0 'id)
(format t "main loaded~%")
