FROM debian:latest

ENV HOME /root

RUN apt-get update
RUN apt-get install -y make libssl-dev
RUN apt-get clean

WORKDIR /root/

ADD ./qvm /root/qvm
ADD ./quil/stdgates.quil /root/stdgates.quil
ADD ./quil/vqe.quil /root/vqe.quil
RUN chmod +x /root/qvm

CMD /root/qvm -S
