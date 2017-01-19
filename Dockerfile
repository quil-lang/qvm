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
RUN chmod +x /root/qvm

CMD /root/qvm --time-limit 30000 --safe-include-directory "/root/quil/"  -S
