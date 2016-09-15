# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 1024

all: qvm

qvm:
	buildapp --output qvm \
		 --dynamic-space-size $(QVM_WORKSPACE) \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system qvm-app \
		 --logfile build-output.log \
		 --entry qvm-app::%main

quilbasic:
	buildapp --output quilbasic \
		 --dynamic-space-size 1024 \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system quil-basic \
		 --logfile build-output.log \
		 --entry quil-basic::%main

# Only works on Linux.
docker: qvm
	docker build -t qvm .

clean:
	rm -f qvm quilbasic build-output.log
