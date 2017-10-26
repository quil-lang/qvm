# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 2048
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


### Testing

testsafe:
	sbcl --dynamic-space-size $(QVM_WORKSPACE) \
		 --noinform --non-interactive \
		 --eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
		 --eval "(sb-ext:restrict-compiler-policy 'debug 3)" \
		 --eval '(ql:quickload :qvm)' \
		 --eval '(asdf:test-system :qvm)'

test:
	sbcl --dynamic-space-size $(QVM_WORKSPACE) \
		 --noinform --non-interactive \
		 --eval '(ql:quickload :qvm)' \
		 --eval '(asdf:test-system :qvm)'

### Cleanup.

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	sbcl --noinform --non-interactive \
             --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

cleanall: clean-cache
	@echo "All cleaned and reindexed."
