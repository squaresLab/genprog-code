FROM ubuntu:18.04

RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common && \
    add-apt-repository -y ppa:avsm/ppa && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      opam \
      ocaml \
      build-essential \
      jq \
      aspcud \
      vim \
      gcc \
      m4 && \
    echo "yes" >> /tmp/yes.txt && \
    opam init --disable-sandboxing -y < /tmp/yes.txt && \
    opam switch create 4.12.0 && \
    opam switch 4.12.0 && \
    opam install -y num && \
    opam pin -y cil https://github.com/squareslab/cil.git

RUN mkdir -p /opt/genprog
WORKDIR /opt/genprog
ADD Makefile Makefile
ADD src src

RUN mkdir bin && \
    eval $(opam config env) && \
    make && \
    make -C src repair.byte && \
    mv src/repair bin/genprog && \
    mv src/repair.byte bin/genprog.byte && \
    ln -s bin/genprog bin/repair && \
    mv src/distserver bin/distserver && \
    mv src/nhtserver bin/nhtserver

ENV PATH "/opt/genprog/bin:${PATH}"

VOLUME /opt/genprog
