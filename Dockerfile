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
    opam switch 4.05.0 && \
    eval $(opam config env) && \
    opam install -y cil

RUN mkdir -p /opt/genprog
WORKDIR /opt/genprog
ADD Makefile Makefile
ADD src src

RUN mkdir bin && \
    make && \
    mv src/repair bin/genprog && \
    ln -s bin/genprog bin/repair && \
    mv src/distserver bin/distserver && \
    mv src/nhtserver bin/nhtserver

ENV PATH "/opt/genprog/bin:${PATH}"

VOLUME /opt/genprog
