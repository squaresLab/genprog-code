FROM ocaml/opam
MAINTAINER Jeremy Lacomis "jlacomis@cmu.edu

USER opam

RUN opam switch 4.05.0
RUN opam install -y cil

RUN mkdir -p /home/opam/genprog
WORKDIR /home/opam/genprog
COPY Makefile Makefile
COPY src src

USER root
RUN chown opam Makefile src
USER opam

RUN eval $(opam config env) && \
    mkdir bin && \
    make && \
    mv src/repair bin/genprog && \
    ln -s bin/genprog bin/repair && \
    mv src/distserver bin/distserver && \
    mv src/nhtserver bin/nhtserver

ENV PATH "/home/opam/genprog/bin:${PATH}"

VOLUME /home/opam/genprog
