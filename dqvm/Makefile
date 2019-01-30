LISP_CACHE ?= `sbcl --noinform --non-interactive --eval '(princ asdf:*user-cache*)'`
QVM_WORKSPACE ?= 800

all: dqvm

dqvm:
	buildapp --output dqvm \
		 --dynamic-space-size $(QVM_WORKSPACE) \
                 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
                 --asdf-tree "./../../" \
                 --eval "(setf sb-ext:\*on-package-variance\* '(:warn (:swank :swank-backend :swank-repl) :error t))" \
                 --load-system dqvm \
                 --compress-core \
                 --logfile build-output.log \
                 --entry dqvm::%main

### Cleanup.

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "!!! Deleting $(LISP_CACHE)"
	sbcl --noinform --non-interactive \
             --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

clean-quicklisp:
	@echo "!!! Cleaning up old projects in Quicklisp"
	sbcl --noinform --non-interactive \
             --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

# Clean the executables
clean:
	rm -f dqvm build-output.log

cleanall: clean clean-cache clean-quicklisp
	@echo "!!! All cleaned and reindexed."
