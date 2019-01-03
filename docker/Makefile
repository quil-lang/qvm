# Makefile for Docker images

BASE_TARGET=qvm
DOCKER_TAG=qvm-base

.PHONY: all
all: qvm-base qvm-tests qvm

.PHONY: qvm-base
qvm-base: Dockerfile.base
	docker build --build-arg build_target=$(BASE_TARGET) -t $(DOCKER_TAG) -f Dockerfile.base ../../

.PHONY: qvm-sdk
qvm-sdk: BASE_TARGET=qvm-sdk
qvm-sdk: DOCKER_TAG=qvm-sdk
qvm-sdk: qvm-base

.PHONY: qvm-sdk-barebones
qvm-sdk-barebones: BASE_TARGET=qvm-sdk-barebones
qvm-sdk-barebones: DOCKER_TAG=qvm-sdk-barebones
qvm-sdk-barebones: qvm-base

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
