build: deps
	sbcl --noinform  --eval "(asdf:operate :build-op :fg-snake)" --quit

deps:
	sbcl --noinform --eval '(ql:quickload "snake")' --quit

run: build
	./snake

clean:
	rm snake
