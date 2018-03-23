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

# Set quicklisp dist version
ENV QUICKLISP_DIST_URL http://beta.quicklisp.org/dist/quicklisp/2018-01-31/distinfo.txt
RUN sbcl --noinform --non-interactive \
    --eval "ql-dist::(when (string/= (canonical-distinfo-url (dist \"quicklisp\")) \"$QUICKLISP_DIST_URL\") (install-dist \"$QUICKLISP_DIST_URL\" :prompt nil :replace t))"

# quickload the qvm-tests system
WORKDIR /src/qvm
RUN sbcl --eval "(ql:quickload :qvm-tests)" --quit

# executed on docker run
ENTRYPOINT ["make", "test"]
