FROM ubuntu:16.04
MAINTAINER Chris Timperley "christimperley@googlemail.com"

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      opam \
      ocaml \
      build-essential \
      jq \
      aspcud \
      vim \
      m4 && \
    echo "yes" >> /tmp/yes.txt && \
    opam init -y < /tmp/yes.txt && \
    opam install -y cil

RUN mkdir -p /opt/genprog
WORKDIR /opt/genprog
ADD Makefile Makefile
ADD src src

RUN eval $(opam config env) && \
    make
