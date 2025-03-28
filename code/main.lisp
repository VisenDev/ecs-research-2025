;;;; Imports and settings

(declaim (optimize (debug 3) (safety 3)))
;(load #P"asdf.lisp")
;(load #P"closer-mop/closer-mop.asd")

;;;; ======================= Util =======================

					;(defmacro with-all-accessors (instance &body body)
					;  (let*
					;      ((names (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots instance))))
					;    `(with-accessors 

;; Better Lexical Scope

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro scope (&body body)
    "This functions as a scope similar to a progn.
   However, you can declare local variables at any point using local,ie,
       (local foo 1)"

    (let ((result '())
	  (ignores '())
	  )
      (dolist (line body)
	(if (and (listp line) (equalp (first line) 'local))
	    (push (cdr line) result)
	    (let
		((tmp (gensym)))
	      (push tmp ignores)
	      (push (list tmp line) result))))
      `(let* ,(reverse result)
	 (declare (ignore ,@(cdr ignores))) ;ignore unused temporaries
	 ,(first (first result)) ;return the value of the last expression pushed onto the list
	 )))


  ;; Quick properties access

  (defun nest-list (list)
    "turns a list like (1 2 3 4) into (4 (3 (2 1)))"
    (when (<= (length list) 2)
      (return-from nest-list (reverse list)))
    (list (car (last list)) (nest-list (butlast list))))

  (defun split-property-symbol (symbol)
    "Splits a symbol like foo.bar.bapp into (bapp (bar foo))"
    (scope
      (local str (symbol-name symbol))
      (local substrs (uiop:split-string str :separator "."))
      (local syms (mapcar #'intern substrs))
      (format t "~a" syms) 
      (local symlist (nest-list syms))
      (format t "~a" symlist) 
      symlist
      ))

  (defun is-property-symbol? (symbol)
    (search "." (symbol-name symbol) :test #'string-equal)
    )

  (defun recursively-split-property-symbols (form)
    (loop :for line :in form :collect
			     (case (type-of line)
			       (cons
				(recursively-split-property-symbols line))
			       (symbol 
				(if (is-property-symbol? line)
				    (split-property-symbol line)
				    line))
			       (t line))))

  (defmacro quick-properties (&body body)
    "allows using class.field syntax inside body"
    (first (recursively-split-property-symbols body)))



  ;; Typed defun

  (defun extract-symbol-and-type (sym)
    "breaks foo@type into just (foo type)"
    (unless (symbolp sym)
      (return-from extract-symbol-and-type (list sym 't)))
    (let*
	((str (symbol-name sym))
	 (strs (uiop:split-string str :separator "@"))
	 (type-str (if (> (length strs) 1) (second strs) "T"))
	 (type-sym (intern type-str))
	 (name-sym (intern (first strs))))
      (list name-sym type-sym)))

  (defmacro typed-defun (name params &body body)
    (let*
	((param-meta (mapcar #'extract-symbol-and-type params))
	 (types (mapcar #'second param-meta))
	 (names (mapcar #'first param-meta))
	 (fn-meta (extract-symbol-and-type name))
	 (fn-name (first fn-meta))
	 (return-type (second fn-meta))
	 )
      `(progn
	 (declaim (ftype (function ,types ,return-type) ,fn-name))
	 (defun ,fn-name ,names ,@body))
      ))

  (typed-defun say-hello@t (count@integer)
    (loop :for i :from 0 :below count :do (format t "hello")) nil)

  (say-hello 1)

  

  ;; Better defun
  (defmacro better-defun (name params &body body)
    "supports typed arguments using foo@type, foo.bar field lookups, and local variables"
    (let*
	((docs-found? (and (listp body) (< 0 (length body)) (equalp 'string (type-of (first body)))))
	 (docs (if docs-found? (first body) nil))
	 (body (if docs-found? (cdr body) body)))
      `(typed-defun ,name ,params
	 ,docs
	 (quick-properties
	   (scope
	     ,@body)))))


  (defmacro orelse (fallback expression)
    `(handler-case
	 ,expression
       (t () ,fallback)))
  )

;;;;======================= Vector =======================

(defun make-vec (type)
  (make-array 0 :element-type type :fill-pointer t :adjustable t))

(defun vec-push (self val)
  (vector-push-extend val self))

(defun vec-run-tests ()
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

(defclass sset () (
		   (dense :initarg :dense :accessor dense)
		   (sparse :initform (make-vec 'integer) :accessor sparse)
		   (dense-to-sparse :initform (make-vec 'integer) :accessor dense-to-sparse)))

(defmacro sset-accessors (instance &body body)
  `(with-accessors ((dense dense)
		    (sparse sparse)
		    (dense-to-sparse dense-to-sparse))
       ,instance
     ,@body))

(defun make-sset (type)
  (make-instance 'sset :dense (make-vec type)))

(better-defun sset-get (self@sset i@integer)
  (orelse nil (aref self.dense i)))

(better-defun sset-len@integer (self@sset)
  self.dense.length)

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
(set-componet *ecs* 0 'id)
