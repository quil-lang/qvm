###############################################################################
# QVM tests docker image
###############################################################################
FROM rigetticomputing/lisp-base

# organize the source
# NOTE: only works if alexa, and magicl, cl-ppcre, cl-quil, and qvm are in the same directory
ADD alexa /src/alexa
ADD magicl /src/magicl
ADD cl-ppcre /src/cl-ppcre
ADD cl-quil /src/cl-quil
ADD qvm /src/qvm

# quickload the qvm-tests system
WORKDIR /src/qvm
RUN sbcl --eval "(ql:quickload :qvm-tests)" --quit

ENV LANG C.UTF-8

# executed on docker run
# ENTRYPOINT ["make", "test", "test-ccl", "coverage"]
ENTRYPOINT ["make", "test"]
