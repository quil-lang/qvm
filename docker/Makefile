# Makefile for Docker images

BASE_TARGET=qvm

.PHONY: all
all: qvm-base qvm-tests qvm

.PHONY: qvm-base
qvm-base: Dockerfile.base
	docker build --build-arg build_target=$(BASE_TARGET) -t qvm-base -f Dockerfile.base ../../

.PHONY: qvm-sdk
qvm-sdk: BASE_TARGET=qvm-sdk
qvm-sdk: qvm-base

.PHONY: qvm-tests
qvm-tests: Dockerfile.test
	docker build -t qvm-tests -f Dockerfile.test ../../

.PHONY: qvm
qvm: Dockerfile
	docker build -t qvm -f Dockerfile ../../

.PHONY: test
test: test.sh
	./test.sh

.PHONY: push
push: push.sh
	./push.sh

.PHONY: clean
clean: clean.sh
	./clean.sh
