;;;; ===================== Imports and settings ====================

(declaim (optimize (debug 3) (safety 3)))


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
   (sparse :initform (make-vec t) :accessor sparse)
   (dense-to-sparse :initform (make-vec 'fixnum) :accessor dense-to-sparse)
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
    (aref self.dense (aref self.sparse i))
    )
  )

(defun sset-len (self)
  (quick-properties self.dense.length)
  )

(defun sset-extend (self new-max-index)
  (incf new-max-index)
  (quick-properties
    (when (< self.sparse.length new-max-index)
      (loop :for i :from self.sparse.length :to new-max-index :do
            (vec-push self.sparse nil))
      )
      (assert (null (aref self.sparse new-max-index)))
    )
  )

(declaim (ftype (function (sset fixnum t) t) sset-set))
(defun sset-set (self i val)
  (if
    (null (ignore-errors (aref (sparse self) i)))
    (let* ((len (length (dense self)))
           )
      (sset-extend self i)
      (vec-push (dense self) val)
      (vec-push (dense-to-sparse self) i)
      (assert (= (length (dense self)) 
                 (length (dense-to-sparse self))))
      (setf (aref (sparse self) i) len)
      )
    (setf (aref (dense self) (aref (sparse self) i)) val)))

;(defun sset-set (self i val)
;  (quick-properties
;    (if (null (orelse nil (sset-get self i)))
;      (let*
;        ((new-dense-index (sset-len self))
;         )
;        (vec-push self.dense val)
;        (vec-push self.dense-to-sparse i)
;        (sset-extend self i)
;        (setf (aref self.sparse i) new-dense-index)
;        )
;      (let*
;        ((dense-index (aref self.sparse i))
;         )
;        (setf (aref (dense self) dense-index) val)
;        )
;      )))

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
  ((component-types :initform (make-hash-table)
                    :accessor component-types
                    :documentation "maps each component-symbol to its datatype")
   (component-owners :initform (make-hash-table)
                     :accessor component-owners
                     :documentation "maps each component-symbol to entities with that type")
   (components :initform (make-hash-table) :accessor components)
   (free-ids :initform (make-vec 'fixnum) :accessor free-ids)
   (highest-id :initform 0 :accessor highest-id :type fixnum)
   ))

(declaim (ftype (function (ecs) fixnum) new-entity))
(defun new-entity (ecs)
  "Creates a new entity and returns its id"
  (quick-properties
    (if (<= ecs.free-ids.length 0)
      (let* ((old-highest ecs.highest-id)
             (new-highest (* 2 (1+ old-highest)))
             )
        (loop :for id :from old-highest :below new-highest
              :do (vec-push ecs.free-ids id))
        (setf ecs.highest-id new-highest)
        (new-entity ecs)
        ;old-highest
        ;(format t "adding new ids: ~a~%" ecs.free-ids)
        ;0
        )
        (vector-pop ecs.free-ids)
    )
  )
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
(defun set-component (self id component-symbol value)
  (assert (typep value (get-type-of-component self component-symbol)))
  (sset-set (gethash component-symbol (components self)) id value)
  (symbol-macrolet
    ((entry (gethash component-symbol (component-owners self)))
     )
    (setf entry (adjoin id entry))
    )
  )

(declaim (ftype (function (ecs fixnum keyword) t) get-component))
(defun get-component (self id component-symbol)
  (sset-get (gethash component-symbol (components self)) id)
  )

(declaim (ftype (function (ecs fixnum keyword) t) unset-component))
(defun unset-component (self id component-symbol)
  (sset-remove (gethash component-symbol (components self)) id)
  (delete id (gethash component-symbol (component-owners self)))
  )

(defun find-entities (self &rest cts)
  "returns a list of all entities that have given components"
  (reduce #'intersection
          (loop :for ct :in cts 
                :collect (gethash ct (component-owners self)))
          )
  )



(deftest
  ecs
  (let* ((ecs (make-instance 'ecs))
         (entity (new-entity ecs))
         )
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (define-component ecs :name 'string)
    (testing-expect-equal (get-type-of-component ecs :name) 'string)

    (define-component ecs :health 'number)
    (testing-expect-equal (get-type-of-component ecs :health) 'number)

    (define-component ecs :special 'boolean)

    (set-component ecs entity :health 10)
    (set-component ecs entity :name "john")

    (testing-expect-equal (get-component ecs entity :health) 10)
    (testing-expect-equal (get-component ecs entity :name) "john")

    (unset-component ecs entity :health)
    (testing-expect-error (get-component ecs entity :health))
    (testing-expect-equal (get-component ecs entity :name) "john")

    (testing-expect-error (get-component ecs 10000 :health))
    (testing-expect-error (get-component ecs 1 :foobar))
    (testing-expect-error (get-component ecs 10000 :foobar))


    (let* ((ids (loop :for i :from 0 :to 5000 :collect (new-entity ecs)))
           (specials '())
           )

      (loop :for i :from 0 :below (length ids)
            :for id :in ids
            :do (set-component ecs id :health i)
            :do (set-component ecs id :name "bob")
            :when (= 0 (mod i 500))
            :do (set-component ecs id :special nil)
            :when (= 0 (mod i 500))
            :do (push id specials)
            )
      (testing-expect 
        (loop :for i :from 0 :below (length ids)
              :for id :in ids
              :always (equalp (get-component ecs id :health) i)
              :always (equalp (get-component ecs id :name) "bob")
              ))
      (testing-expect-equal (find-entities ecs :special) specials)
      )

    )
  )



(defparameter *ecs* (make-instance 'ecs))
(defun main()
  (format t "~a~%" *ecs*)
  (define-component *ecs* :name 'integer)
  (define-component *ecs* :id 'integer)
  (format t "~a~%" *ecs*)
  )
;(set-componet *ecs* 0 'id)
;(format t "main loaded~%")
