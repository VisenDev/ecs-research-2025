;;;; ===================== Imports and settings ====================

(declaim (optimize (debug 3) (safety 3)))
(load #p"asdf.lisp")
(load #p"closer-mop/closer-mop.asd")
(load #p"macros.lisp")

;;;;======================= Vector =======================

(defun make-vec (type)
  (make-array 0 :element-type type :fill-pointer t :adjustable t))

(defun vec-push (self val)
  (vector-push-extend val self))

(defun test-vec ()
  "tests to make sure vec works"
  (quick-properties 
    (let ((vec (make-vec 'integer)))
      (assert (= vec.length -0))
      (vec-push vec 1)
      (assert (= vec.length 1))
      (loop :for i :from 0 :below 10 :do
            (vec-push vec i))
      (assert (= vec.length 11))
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
    (orelse nil (aref self.dense i))
    ))

(defun sset-len (self)
  (quick-properties self.dense.length)
  )

(defun sset-extend (self new-max-index)
  (quick-properties
    (unless (< self.sparse.len new-max-index)
      (loop :for i :from self.sparse.len :to new-max-index :do
            (vec-push self.sparse -1))
      )))

(defun sset-set (self i val)
  (quick-properties
    (assert (not val.null))
    (if (null (sset-get self i))
      (let* ((new-dense-index (sset-len self))
         )
        (vec-push self.dense val)
        (vec-push self.dense-to-sparse i)
        (sset-extend self i)
        (setf (aref self.sparse i) new-dense-index)
        )
      (let*
        ((dense-index (aref self.sparse ))
        )
        (setf (aref (dense self) dense-index) val))
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
    (unless (null (get-type-of-component ecs component-symbol))
      (error "component ~a already defined as having type ~a"
             component-symbol
             (get-type-of-component ecs component-symbol)
             ))
    (setf (gethash component-symbol ecs.component-types ecs) component-type)
    (setf
      (gethash component-symbol ecs.components) 
      (make-array 16 :element-type component-type :fill-pointer t :adjustable t)
      )
    ))

(defun set-component (ecs component-symbol value)
  (unless (equalp (component-type ecs component-symbol) (type-of value))
    (error "component ~a expects type ~a but was given value of type ~a"
	   component-symbol
	   (component-type ecs component-symbol)
	   (type-of value)
	   )
    )
  )


(defvar *ecs* (make-instance 'ecs))
(format t "~a~%" *ecs*)
(define-component *ecs* 'name 'integer)
(define-component *ecs* 'id 'integer)
(format t "~a~%" *ecs*)
;(set-componet *ecs* 0 'id)
