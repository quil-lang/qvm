###############################################################################
# QVM tests docker image (docker.lab.rigetti.com/qcs/qvm-tests)
###############################################################################
FROM docker.lab.rigetti.com/qcs/lisp-base

# organize the source
# NOTE: this only works if the current directory is the top-level directory of qvm and
# the alexa, cl-quil, and magicl sources are in subdirectories inside qvm
ADD . /src/qvm
RUN cp -R /src/qvm/alexa /src/alexa
RUN rm -rf /src/qvm/alexa
RUN cp -R /src/qvm/cl-quil /src/cl-quil
RUN rm -rf /src/qvm/cl-quil
RUN cp -R /src/qvm/magicl /src/magicl
RUN rm -rf /src/qvm/magicl

# update quicklisp
RUN sbcl --eval "(ql:update-all-dists :prompt nil)" --quit

# quickload the qvm-tests system
WORKDIR /src/qvm
RUN sbcl --eval "(ql:quickload :qvm-tests)" --quit

# executed on docker run
ENTRYPOINT ["make", "test"]
