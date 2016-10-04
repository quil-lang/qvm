# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 1024
LISP_CACHE ?= `sbcl --noinform --non-interactive --eval '(princ asdf:*user-cache*)'`

all: qvm

### Some basic one-time or rare commands.

# Download and install Quicklisp.
quicklisp:
	curl -o /tmp/quicklisp.lisp "http://beta.quicklisp.org/quicklisp.lisp"
	sbcl --noinform --non-interactive \
	     --load /tmp/quicklisp.lisp \
	     --eval '(quicklisp-quickstart:install)'
	echo >> ~/.sbclrc
	echo '#-quicklisp(let ((i(merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))(when(probe-file i)(load i)))' >> ~/.sbclrc
	echo "#+quicklisp(push \"$(shell pwd | xargs dirname)/\" ql:*local-project-directories*)" >> ~/.sbclrc
	rm -f /tmp/quicklisp.lisp


# Download all dependencies, updating Quicklisp.
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



### Actual executable building.

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



### Deployment.

# Only works on Linux.
docker: qvm
	docker build -t qvm .



### Cleanup.

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	sbcl --noinform --non-interactive \
	     --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

# Clean the executables
clean:
	rm -f qvm quilbasic build-output.log

cleanall: clean clean-cache
	@echo "All cleaned and reindexed."
