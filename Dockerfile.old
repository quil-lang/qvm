FROM debian:latest

ENV HOME /root

RUN apt-get update
RUN apt-get install -y make libssl-dev
RUN apt-get clean

WORKDIR /root/

ADD ./qvm /root/qvm
RUN mkdir /root/quil/
ADD ./quil/stdgates.quil /root/quil/stdgates.quil
ADD ./quil/vqe.quil /root/quil/vqe.quil
ADD ./quil/other.quil /root/quil/other.quil
ADD ./quil/circs.quil /root/quil/circs.quil
RUN chmod +x /root/qvm

CMD /root/qvm -S
