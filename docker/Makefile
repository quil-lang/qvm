# Makefile for Docker images

.PHONY: all
all: qvm-tests

.PHONY: qvm-tests
qvm-tests: Dockerfile
	docker build -t qvm-tests -f Dockerfile ../../

.PHONY: test
test: test.sh
	./test.sh

.PHONY: push
push: push.sh
	./push.sh

.PHONY: clean
clean: clean.sh
	./clean.sh
