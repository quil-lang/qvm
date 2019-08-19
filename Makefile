# Heap space for QVM in MiB.
QVM_WORKSPACE ?= 2048
LISP_CACHE ?= `sbcl --noinform --non-interactive --eval '(princ asdf:*user-cache*)'`

COMMIT_HASH=$(shell git rev-parse --short HEAD)
RIGETTI_LISP_LIBRARY_HOME=../

SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --dynamic-space-size $(QVM_WORKSPACE) --noinform --non-interactive --no-userinit --no-sysinit

QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval '(push :hunchentoot-no-ssl *features*)' \
	--eval '(push :drakma-no-ssl *features*)' \
	--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)"

QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp

PREFIX ?= /usr/local

all: qvm

###############################################################################
# SETUP
###############################################################################

$(QUICKLISP_SETUP):
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

###############################################################################
# DEPENDENCIES
###############################################################################

dump-version-info:
	$(QUICKLISP) \
		--eval '(format t "~A ~A" (lisp-implementation-type) (lisp-implementation-version))' \
		--eval '(print (ql-dist:find-system "magicl"))' \
		--eval '(print (ql-dist:find-system "rpcq"))' \
		--eval '(print (ql-dist:find-system "quilc"))' \
		--eval '(terpri)' --quit

###############################################################################
# BUILD
###############################################################################

# FOREST_SDK_OPTION *must* come last - it triggers the end of normal
# SBCL option processing
qvm: system-index.txt
	$(SBCL) $(FOREST_SDK_FEATURE) \
	        --eval "(setf sb-ext:\*on-package-variance\* '(:warn (:swank :swank-backend :swank-repl) :error t))" \
		--eval '(push :hunchentoot-no-ssl *features*)' \
		--eval '(push :drakma-no-ssl *features*)' \
		--load build-app.lisp \
                $(FOREST_SDK_OPTION)

qvm-sdk-base: FOREST_SDK_FEATURE=--eval '(pushnew :forest-sdk *features*)'
qvm-sdk-base: QVM_WORKSPACE=10240
qvm-sdk-base: clean clean-cache qvm

# By default, relocate shared libraries on SDK builds
qvm-sdk: FOREST_SDK_OPTION=--qvm-sdk
qvm-sdk: qvm-sdk-base

# Don't relocate shared libraries on barebones SDK builds
qvm-sdk-barebones: qvm-sdk-base

DOCKER_BUILD_TARGET=all
DOCKER_TAG=rigetti/qvm:$(COMMIT_HASH)
.PHONY: docker
docker: Dockerfile
	docker build --build-arg build_target=$(DOCKER_BUILD_TARGET) \
		-t $(DOCKER_TAG) .

docker-sdk: DOCKER_BUILD_TARGET=qvm-sdk
docker-sdk: DOCKER_TAG=qvm-sdk
docker-sdk: docker

docker-sdk-barebones: DOCKER_BUILD_TARGET=qvm-sdk-barebones
docker-sdk-barebones: DOCKER_TAG=qvm-sdk-barebones
docker-sdk-barebones: docker

###############################################################################
# INSTALL/UNINSTALL
###############################################################################

 .PHONY: install
install: qvm
	install qvm $(DESTDIR)$(PREFIX)/bin

 .PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/qvm

###############################################################################
# TEST
###############################################################################

testsafe:
	sbcl --dynamic-space-size $(QVM_WORKSPACE) \
		 --noinform --non-interactive \
		 --eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
		 --eval "(sb-ext:restrict-compiler-policy 'debug 3)" \
		 --eval '(ql:quickload :qvm-tests)' \
		 --eval '(asdf:test-system :qvm)'

test: test-lib test-app

test-lib:
	$(QUICKLISP) \
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

###############################################################################
# CLEAN
###############################################################################

# Clean the executables
clean:
	rm -f qvm build-output.log system-index.txt

# Clean the Lisp cache, reindex local projects.
clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	$(QUICKLISP) \
             --eval "(ql:register-local-projects)"
	rm -rf $(LISP_CACHE)

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) \
             --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

cleanall: clean clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."

###############################################################################
# QUICKLISP UTILITES
###############################################################################

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

# Update Quicklisp.
deps: $(QUICKLISP_SETUP)
	rm -f system-index.txt
	$(QUICKLISP) --eval '(ql:update-client :prompt nil)'
	$(QUICKLISP) --eval '(ql:update-dist "quicklisp" :prompt nil)'
