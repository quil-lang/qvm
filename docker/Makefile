# Makefile for Docker images

.PHONY: qvm-tests
qvm-tests:
	docker build -t docker.lab.rigetti.com/qcs/qvm-tests -f Dockerfile ../

.PHONY: clean
clean:
	./cleanup.sh