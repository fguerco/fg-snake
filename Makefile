build:
	sbcl --eval "(asdf:operate :build-op :snake)" --quit

run: build
	./snake

clean:
	rm snake
