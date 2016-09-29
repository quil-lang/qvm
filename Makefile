# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 1024

all: qvm

deps:
        # Update Quicklisp.
	sbcl --noinform --non-interactive \
	     --eval "(ql:update-client :prompt nil)"
        # Update Quicklisp software.
	sbcl --noinform --non-interactive \
	     --eval "(ql:update-all-dists :prompt nil)"
        # Update QVM, etc.
	sbcl --noinform --non-interactive \
	     --load "qvm.asd" \
	     --load "qvm-app.asd" \
	     --load "quil-basic/quil-basic.asd" \
	     --eval "(ql:quickload '(:qvm-app :quil-basic))"

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
		 --dynamic-space-size $(QVM_WORKSPACE) \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
		 --asdf-tree "./../" \
		 --load-system quil-basic \
		 --load "./quil-basic/zap-info.lisp" \
		 --eval '(zap-info)' \
		 --logfile build-output.log \
		 --entry quil-basic::%main

# Only works on Linux.
docker: qvm
	docker build -t qvm .

clean:
	rm -f qvm quilbasic build-output.log
