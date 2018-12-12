# Makefile for Docker images

BASE_TARGET=qvm

.PHONY: all
all: qvm-tests qvm-app-base qvm-app-tests qvm

.PHONY: qvm-app-base
qvm-app-base: Dockerfile.base
	docker build --build-arg build_target=$(BASE_TARGET) -t qvm-app-base -f Dockerfile.base ../../

.PHONY: qvm-app-sdk
qvm-app-sdk: BASE_TARGET=qvm-sdk
qvm-app-sdk: qvm-app-base


.PHONY: qvm-app-tests
qvm-app-tests: Dockerfile.test
	docker build -t qvm-app-tests -f Dockerfile.test ../../

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

# # Makefile for Docker images

# .PHONY: all
# all: qvm-tests

.PHONY: qvm-tests
qvm-tests: Dockerfile
	docker build -t qvm-tests -f Dockerfile ../../

# .PHONY: test
# test: test.sh
# 	./test.sh

# .PHONY: push
# push: push.sh
# 	./push.sh

# .PHONY: clean
# clean: clean.sh
# 	./clean.sh
