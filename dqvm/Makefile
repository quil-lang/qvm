QVM_WORKSPACE ?= 800

SBCL_BIN ?= sbcl
SBCL := $(SBCL_BIN) --noinform --non-interactive
LISP_CACHE ?= `$(SBCL) --eval '(princ asdf:*user-cache*)'`

SOURCE_DIR := src

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)'

.PHONY: all
all: dqvm

dqvm: dqvm.asd $(wildcard $(SOURCE_DIR)/*.lisp)
	buildapp --output dqvm \
		 --dynamic-space-size $(QVM_WORKSPACE) \
                 --asdf-tree "~/quicklisp/dists/quicklisp/software/" \
                 --asdf-tree "./../../" \
                 --eval "(setf sb-ext:\*on-package-variance\* '(:warn (:swank :swank-backend :swank-repl) :error t))" \
                 --load-system dqvm \
                 --logfile build-output.log \
                 --entry dqvm::%main

### Cleanup.
.PHONY: clean-cache clean-quicklisp clean cleanall

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	$(QUICKLISP) --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

# Clean the executables
clean:
	rm -f dqvm build-output.log

cleanall: clean clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
