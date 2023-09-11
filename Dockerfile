FROM ubuntu:16.04
COPY src /src/
WORKDIR /src
RUN DEBIAN_FRONTEND=noninteractive && apt-get update && apt-get install -y build-essential opam gcc-multilib wget m4 autoconf libz-dev flex bison subversion
RUN opam init --yes
RUN opam switch 3.12.1 && opam switch set 3.12.1 && eval `opam config env` && opam install ocamlfind ocamlbuild batteries num ounit --yes &&  make
ENTRYPOINT ["/bin/bash"]

