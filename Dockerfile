FROM ocaml/opam
MAINTAINER Jeremy Lacomis "jlacomis@cmu.edu

USER opam

RUN opam switch 4.05.0 && \
    eval $(opam config env)

RUN opam install -y cil

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
