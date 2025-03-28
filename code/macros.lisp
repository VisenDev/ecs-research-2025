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
    (let*
      ((str (symbol-name symbol))
      (substrs (uiop:split-string str :separator "."))
      (syms (mapcar #'intern substrs))
      (symlist (nest-list syms)))
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
