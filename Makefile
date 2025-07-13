lisp = sbcl --noinform --quit  --eval

build: deps
	$(lisp) "(asdf:operate :build-op :fg-snake)"

deps:
	$(lisp) '(ql:quickload "fg-snake")'

run: build
	./fg-snake

clean:
	rm fg-snake
