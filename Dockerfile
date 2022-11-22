FROM ubuntu:20.04

# build variables
ARG QUICKLISP_VERSION=2022-04-01
ARG QUICKLISP_URL=http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_VERSION}/distinfo.txt

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
    && apt-get install -y \
    curl \
    wget \
    git \
    build-essential \
    cmake \
    libblas-dev \
    libffi-dev \
    liblapack-dev \
    libz-dev \
    libzmq3-dev \
    rlwrap \
    sbcl \
    ca-certificates \
    sed


WORKDIR /src

RUN wget -P /tmp/ 'https://beta.quicklisp.org/quicklisp.lisp' \
    && sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp \
    --eval "(quicklisp-quickstart:install :dist-url \"${QUICKLISP_URL}\")" \
    && sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
    && echo '#+quicklisp(push "/src" ql:*local-project-directories*)' >> ~/.sbclrc \
    && rm -f /tmp/quicklisp.lisp


RUN git clone https://github.com/quil-lang/quilc.git
RUN cd ~/quicklisp/local-projects/ && git clone https://github.com/quil-lang/magicl.git
RUN cd ~/quicklisp/local-projects/ && git clone https://github.com/cffi/cffi.git

RUN sed -i '74s/#-x86-64/#+ARM64/' ~/quicklisp/local-projects/cffi/libffi/libffi-types.lisp
RUN sed -i '76s/#+x86-64/#-ARM64/' ~/quicklisp/local-projects/cffi/libffi/libffi-types.lisp

# build the quilc app
COPY . /src/qvm
WORKDIR /src/qvm

RUN git clean -fdx && make ${build_target} install

EXPOSE 5000

ENTRYPOINT ["./qvm"]
