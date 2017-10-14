###############################################################################
# QVM docker image (docker.lab.rigetti.com/qcs/qvm)
###############################################################################
FROM docker.lab.rigetti.com/qcs/lisp-base

###############################################################################
# quilc / QVM / wildfire dependencies setup
###############################################################################

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

# qvm deps (needs git)
WORKDIR /src/qvm
RUN make deps

###############################################################################
# QVM setup
###############################################################################

# install zmq
RUN apt-get install -y libzmq3-dev

# build QVM (needs zmq)
WORKDIR /src/qvm
RUN sbcl --eval "(ql:quickload :qvm)" --quit
RUN make

# executed on docker run
ENTRYPOINT ["./qvm", "-S"]
