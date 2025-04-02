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

(declaim (ftype (function (sset fixnum) t) sset-remove))
(defun sset-remove (self i)
  (quick-properties
    ;(when (= 1 self.dense.length)
    ;  (setf (aref self.sparse i) nil)
    ;  (return-from sset-remove (vector-pop self.dense)))

    (let* ((top-dense-index (1- self.dense.length))
           (tmp1 (assert (= self.dense.length self.dense-to-sparse.length)))
           (top-sparse-index (aref self.dense-to-sparse top-dense-index))
           (top-value (aref self.dense top-dense-index))
           (deleted-dense-index (aref self.sparse i))
           (tmp2 (assert (not (null deleted-dense-index))))
           (deleted-value (aref self.dense deleted-dense-index))
           )
      (declare (ignore tmp1 tmp2))

      ;;replace deleted index with top value
      (setf (aref self.dense deleted-dense-index) top-value)
      
      ;;update top-value's sparse index to its new location
      (setf (aref self.sparse top-sparse-index) deleted-dense-index)

      ;;remove the deleted value's sparse index
      (setf (aref self.sparse i) nil)

      ;;remove the top element of the dense array
      (vector-pop self.dense)
      (vector-pop self.dense-to-sparse)

      ;;return deleted value
      deleted-value
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
      (testing-expect-equal (sset-get sset 10) 'bar)

      (sset-set sset 100 'var)
      (testing-expect-equal (sset-len sset) 2)
      (testing-expect-equal (sset-get sset 10) 'bar)
      (testing-expect-equal (sset-get sset 100) 'var)

      (sset-remove sset 10)
      (testing-expect-equal (sset-len sset) 1)
      (testing-expect-equal (sset-get sset 100) 'var)

      (sset-set sset 1 'hello)
      (sset-set sset 2 'world)
      (sset-set sset 3 'asdf)
      (sset-set sset 4 'john)
      (testing-expect-equal (sset-len sset) 5)
      (testing-expect-equal (sset-get sset 100) 'var)
      (testing-expect-equal (sset-get sset 1) 'hello)
      (testing-expect-equal (sset-get sset 2) 'world)
      (testing-expect-equal (sset-get sset 3) 'asdf)
      (testing-expect-equal (sset-get sset 4) 'john)

      (sset-remove sset 3)
      (testing-expect-equal (sset-len sset) 4)
      (testing-expect-equal (sset-get sset 100) 'var)
      (testing-expect-equal (sset-get sset 1) 'hello)
      (testing-expect-equal (sset-get sset 2) 'world)
      (testing-expect-equal (sset-get sset 4) 'john)
    )
  )






;;;; ======================= ECS =======================

(defclass ecs ()
  ((component-types :initform (make-hash-table) :accessor component-types)
   (components :initform (make-hash-table) :accessor components)
   (free-ids :initform (make-vec 'fixnum) :accessor free-ids)
   (highest-id :initform 0 :accessor highest-id :type fixnum)
   ))

(declaim (ftype (function (ecs) fixnum) new-entity))
(defun new-entity (ecs)
  "Creates a new entity and returns its id"
  (quick-properties
    (when (<= ecs.free-ids.length 0)
      (let* ((old-highest ecs.highest-id)
             (new-highest (1+ (* 2 old-highest)))
             )
        (loop :for id :from old-highest :below new-highest
              :do (vec-push ecs.free-ids id))
        (setf ecs.highest-id new-highest)
        ))
    (vector-pop ecs.free-ids))
  )

(declaim (ftype (function (ecs keyword) symbol) get-type-of-component))
(defun get-type-of-component (self component-symbol)
    (gethash component-symbol (component-types self))
    )

(declaim (ftype (function (ecs keyword symbol) t) define-component))
(defun define-component (ecs component-symbol component-type)
  (assert (not (null component-type)))
  (assert (not (null component-symbol)))
  ;(quick-properties
  ;  (unless (null (get-type-of-component ecs component-symbol))
  ;    (error "component ~a already defined as having type ~a instead of ~a"
  ;           component-symbol
  ;           (get-type-of-component ecs component-symbol)
  ;           component-type
  ;           ))
    ;(print-magenta "The value of component-type is ~a~%~%"  component-type)
    ;(print-magenta "The value of component-symbol is ~a~%~%"  component-symbol)
    (setf (gethash component-symbol (component-types ecs)) component-type)
    (assert (equalp component-type (gethash component-symbol (component-types ecs))))
    (assert (equalp component-type (get-type-of-component ecs component-symbol)))
    (setf
      (gethash component-symbol (components ecs)) 
      (make-sset component-type)
      ;(make-array 16 :element-type component-type :fill-pointer t :adjustable t)
      )
    
    (assert (equalp component-type (gethash component-symbol (component-types ecs))))
    (assert (equalp component-type (get-type-of-component ecs component-symbol)))
  )

(declaim (ftype (function (ecs fixnum keyword t) t) set-component))
(defun set-component (self entity component-symbol value)
  (assert (not (null (get-type-of-component self component-symbol))))
  ;(unless (equalp (get-type-of-component self component-symbol) (type-of value))
  ;  (error "component ~a expects type ~a but was given value of type ~a"
  ;         component-symbol
  ;         (get-type-of-component self component-symbol)
  ;         (type-of value)
  ;         )
  ;  )
  (quick-properties
    (sset-set (gethash component-symbol self.components) entity value)
    )
  )


(deftest
  ecs
  (let* ((ecs (make-instance 'ecs))
         (entity (new-entity ecs))
         )
    (define-component ecs :name 'string)
    (testing-expect-equal (get-type-of-component ecs :name) 'string)

    (define-component ecs :health 'number)
    (testing-expect-equal (get-type-of-component ecs :health) 'number)

    (set-component ecs entity :health 10)
    (set-component ecs entity :name "john")

    (testing-expect-error (error "error"))
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
;(format t "main loaded~%")
