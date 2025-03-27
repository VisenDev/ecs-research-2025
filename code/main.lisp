;;;; Imports and settings

(declaim (optimize (debug 3) (safety 3)))
(load #P"asdf.lisp")
(load #P"closer-mop/closer-mop.asd")

;;;; Util
;(defmacro with-all-accessors (instance &body body)
;  (let*
;      ((names (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots instance))))
;    `(with-accessors 

  

;;;; Vector

(defun make-vec (type)
  (make-array 0 :element-type type :fill-pointer t :adjustable t))

(defun vec-push (self val)
  (vector-push-extend self val))

;;;; Sparse Set

(defclass sset () (
    (dense :initarg :dense :accessor dense)
    (sparse :initform (make-vec 'integer) :accessor sparse)
    (dense-to-sparse :initform (make-vec 'integer) :accessor dense-to-sparse)))

(defmacro defun-sset (name params &body body)
  (unless (equalp (car params) 'self)
    (error "the first element of params should be called \"self\""))
  `(defun ,name ,params
     ;(declaim (ftype (function (sset)) ,name))
     (with-accessors self ((dense dense)
			   (sparse sparse)
			   (dense-to-sparse dense-to-sparse))
       ,body)))

(defmacro sset-accessors (instance &body body)
  `(with-accessors ((dense dense)
		    (sparse sparse)
		    (dense-to-sparse dense-to-sparse))
       ,instance
       ,@body))

(defun make-sset (type)
  (make-instance 'sset :dense (make-vec type)))

;(defun sset-get (self i)
;  (handler-case
;      (let ((index (aref (sparse self) i)))
;	(aref (dense self) index))
;    (t () nil)))

(defun sset-get (self i)
  (sset-accessors self
    (handler-case 
	(let ((index (aref sparse i)))
	    (aref dense index))
	(t () nil)))
  )

(defun sset-len (self)
  (length (dense self)))

(defun sset-extend (self new-max-index)
  (when (> (length (sparse self)) new-max-index)
    (return-from sset-extend))
  (loop :for i from (length (sparse self)) to new-max-index :do
    (progn
      (vec-push (sparse self) -1))))

(defun sset-set (self i val)
  (when (null val)
    (error "value provided to sset-set cannot be null"))
  (if (null (sset-get self i))
    (let*
	((dense-index (sset-len self))
	 )
      (vec-push (dense self) val)
      (vec-push (dense-to-sparse self) i)
      (sset-extend self i)
      (setf (aref (sparse self) i) dense-index)
      )
    (let*
	((dense-index (aref (sparse self) i)))
      (setf (aref (dense self) dense-index) val))))

(defun sset-remove (self i)
  (when (null (sset-get self i))
    (return-from sset-remove))
  (with-accessors self ()
  (let*
      ((dense-index (aref (sparse self) i))
       (top-i (aref (
       )
    (setf (aref (sparse self) i) -1)
    (
  
	   

    
	  


;;;; ECS

(defstruct ecs 
   (component-types (make-hash-table))
   (components (make-hash-table))
   (ids '())
   (highest-id 0)
)

(defun component-type (ecs component-symbol) 
  (let ((result (gethash component-symbol (ecs-component-types ecs))))
    (when (equalp result nil)
      (error "component-type ~a is undefined" component-symbol)
    )
  )
)

(defun component-defined? (ecs component-symbol) 
  (not (not (component-type ecs component-symbol)))
)

(defun define-component (ecs component-symbol component-type)
  (when (component-defined? ecs component-symbol)
    (error "component ~a already defined" component-symbol)
  )
  (setf (gethash component-symbol (ecs-component-types ecs)) component-type)
  (setf (gethash component-symbol (ecs-components ecs)) 
        (make-array 16 :element-type component-type :fill-pointer t :adjustable t)
  )
)

(defun set-component (ecs component-symbol value)
  (unless (equalp (component-type ecs component-symbol) (type-of value))
    (error "component ~a expects type ~a but was given value of type ~a"
      component-symbol
      (component-type ecs component-symbol)
      (type-of value)
    )
  )
)


(defvar *ecs* (make-ecs))
(format t "~a~%" *ecs*)
(define-component *ecs* 'name 'integer)
(define-component *ecs* 'id 'integer)
(format t "~a~%" *ecs*)
(set-componet *ecs* 0 'age 
