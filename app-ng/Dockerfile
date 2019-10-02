# TODO(appleby): This Dockerfile should eventually be merged with the toplevel Dockerfile.
# specify the dependency versions (can be overriden with --build_arg)
ARG quilc_version=1.11.1
ARG quicklisp_version=2019-07-11

# use multi-stage builds to independently pull dependency versions
FROM rigetti/quilc:$quilc_version as quilc
FROM rigetti/lisp:$quicklisp_version

# copy over quilc source from the first build stage
COPY --from=quilc /src/quilc /src/quilc

ARG build_target=qvm-ng

# build the qvm-ng app
ADD . /src/qvm
WORKDIR /src/qvm
RUN git clean -fdx && make ${build_target}

EXPOSE 5222

ENTRYPOINT ["./qvm-ng"]
CMD ["-S"]
