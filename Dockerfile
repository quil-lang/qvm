FROM rigetti/quilc

# build the qvm app
ADD . /src/qvm
WORKDIR /src/qvm
RUN git clean -fdx && make

ENTRYPOINT ["./qvm"]
