FROM debian:latest

ENV HOME /root

RUN apt-get update
RUN apt-get install -y make libssl-dev
RUN apt-get clean

WORKDIR /root/

# Install Quicklisp
ADD ./qvm /root/qvm
RUN chmod +x /root/qvm

CMD /root/qvm
