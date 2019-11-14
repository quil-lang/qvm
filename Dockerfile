# specify the dependency versions (can be overriden with --build_arg)
ARG quilc_version=1.13.0
ARG quicklisp_version=2019-07-11

# use multi-stage builds to independently pull dependency versions
FROM rigetti/quilc:$quilc_version as quilc
FROM rigetti/lisp:$quicklisp_version

# copy over quilc source from the first build stage
COPY --from=quilc /src/quilc /src/quilc

ARG build_target

# build the qvm app
ADD . /src/qvm
WORKDIR /src/qvm
RUN git clean -fdx && make ${build_target}

EXPOSE 5000

ENTRYPOINT ["./qvm"]
