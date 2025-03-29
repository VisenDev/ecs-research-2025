all: *.lisp Makefile
	sbcl --disable-debugger --script main.lisp

compile: *.lisp Makefile
	./build.lisp

.PHONY test:
test: *.lisp
	sbcl --no-userinit --load main.lisp --eval "(run-tests)" --quit

.PHONY clean:
clean:
	@trash *.fasl *.fas
