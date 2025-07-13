build: deps
	sbcl --noinform  --eval "(asdf:operate :build-op :fg-snake)" --quit

deps:
	sbcl --noinform --eval '(ql:quickload "fg-snake")' --quit

run: build
	./snake

clean:
	rm snake
