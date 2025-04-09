#all: *.lisp Makefile
#	sbcl --eval "(progn (asdf:load-system :ecs-research-2025) (sb-ext:save-lisp-and-die \"a.out\" :toplevel #'main :executable t))"
#
#ecl: *.lisp
#	ecl --eval "(asdf:make :ecs-research-2025/executable :force t)(quit)"

a.out: *.lisp
	sbcl --eval "(asdf:make :ecs-research-2025/executable :force t)" --quit

#.PHONY test:
#test: *.lisp
#	sbcl --no-userinit --load main.lisp --eval "(run-tests)" --quit

.PHONY clean:
clean:
	@trash a.out
