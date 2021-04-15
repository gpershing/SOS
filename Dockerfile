# Based on 20.04 LTS
FROM ubuntu:focal

# Set timezone:
RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone

RUN apt-get -yq update && \
    apt-get -y upgrade && \
    apt-get -yq --no-install-suggests --no-install-recommends install \
    ocaml \
    menhir \
    llvm-10 \
    llvm-10-dev \
    m4 \
    git \
    aspcud \
    ca-certificates \
    python2.7 \
    pkg-config \
    cmake \
    opam

RUN ln -s /usr/bin/lli-10.0 /usr/bin/lli
RUN ln -s /usr/bin/llc-10.0 /usr/bin/llc

RUN opam init --disable-sandboxing -vv
RUN opam install \
    llvm.10.0.0 \
    ocamlfind -y
RUN eval `opam config env`

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]
