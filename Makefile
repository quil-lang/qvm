# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 2048
LISP_CACHE ?= `sbcl --noinform --non-interactive --eval '(princ asdf:*user-cache*)'`

RIGETTI_LISP_LIBRARY_HOME=../

SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --non-interactive --no-userinit --no-sysinit

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval '(push :hunchentoot-no-ssl *features*)' \
	--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)"

QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp

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

$(QUICKLISP_SETUP):
	rm -f system-index.txt
	mkdir -p $(QUICKLISP_HOME)
	curl -o $(QUICKLISP_HOME)/quicklisp-bootstrap.lisp \
		$(QUICKLISP_BOOTSTRAP_URL)
	$(SBCL) --load $(QUICKLISP_HOME)/quicklisp-bootstrap \
		--eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)\")"

system-index.txt: $(QUICKLISP_SETUP)
	$(QUICKLISP) \
		$(FOREST_SDK_FEATURE) \
		--eval '(ql:quickload "cffi-grovel")' \
		--eval '(ql:quickload "qvm-app")' \
		--eval '(ql:write-asdf-manifest-file "system-index.txt")'

# Update Quicklisp.
deps: $(QUICKLISP_SETUP)
	rm -f system-index.txt
	$(QUICKLISP) --eval '(ql:update-client :prompt nil)'
	$(QUICKLISP) --eval '(ql:update-dist "quicklisp" :prompt nil)'

qvm: system-index.txt
	buildapp --output qvm \
                 --dynamic-space-size $(QVM_WORKSPACE) \
                 --manifest-file system-index.txt \
                 --eval "(setf sb-ext:\*on-package-variance\* '(:warn (:swank :swank-backend :swank-repl) :error t))" \
                 --eval '(push :hunchentoot-no-ssl *features*)' \
		 --asdf-path . \
                 --load-system qvm-app \
		 $(FOREST_SDK_LOAD) \
                 --eval '(qvm-app::zap-info)' \
                 --eval '(qvm-app::setup-debugger)' \
                 --compress-core \
                 --logfile build-output.log \
                 --entry qvm-app::%main

qvm-sdk: FOREST_SDK_FEATURE=--eval '(pushnew :forest-sdk *features*)'
qvm-sdk: QVM_WORKSPACE=10240
qvm-sdk: FOREST_SDK_LOAD=--load app/src/mangle-shared-objects.lisp
qvm-sdk: clean clean-cache qvm

### Testing

testsafe:
	sbcl --dynamic-space-size $(QVM_WORKSPACE) \
		 --noinform --non-interactive \
		 --eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
		 --eval "(sb-ext:restrict-compiler-policy 'debug 3)" \
		 --eval '(ql:quickload :qvm-tests)' \
		 --eval '(asdf:test-system :qvm)'

test: test-lib test-app

test-lib:
	sbcl --dynamic-space-size $(QVM_WORKSPACE) \
		 --noinform --non-interactive \
		 --eval '(ql:quickload :qvm-tests)' \
		 --eval '(asdf:test-system :qvm)'

test-app:
	$(QUICKLISP) \
		 --eval '(ql:quickload :qvm-app-tests)' \
		 --eval '(asdf:test-system :qvm-app)'

test-ccl:
	ccl --batch --eval '(ql:quickload :qvm)' --eval '(quit)'

coverage:
	sbcl --noinform --non-interactive --load coverage-report/coverage-report.lisp


### Cleanup.

# Clean the executables
clean:
	rm -f qvm build-output.log system-index.txt

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	sbcl --noinform --non-interactive \
             --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) \
             --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

cleanall: clean clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
