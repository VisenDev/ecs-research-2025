#!/usr/bin/env sbcl --script

(declaim (optimize (debug 3)))

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
