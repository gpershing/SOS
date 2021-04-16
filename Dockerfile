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
    opam \
    meson \
    python3 \
    python3-distutils

RUN ln -s /usr/bin/lli-10.0 /usr/bin/lli
RUN ln -s /usr/bin/llc-10.0 /usr/bin/llc

RUN opam init --disable-sandboxing -vv
RUN opam install \
    llvm.10.0.0 \
    ocamlfind -y
RUN eval `opam config env`

RUN wget https://bootstrap.pypa.io/get-pip.py
RUN python3 get-pip.py
RUN pip install Mako

RUN apt-get install -y libdrm-dev libxxf86vm-dev libxt-dev xutils-dev flex bison xcb libx11-xcb-dev libxcb-glx0 libxcb-glx0-dev xorg-dev libxcb-dri2-0-dev
RUN apt-get install -y libelf-dev libunwind-dev valgrind libwayland-dev wayland-protocols libwayland-egl-backend-dev
RUN apt-get install -y libxcb-shm0-dev libxcb-dri3-dev libxcb-present-dev libxshmfence-dev
RUN apt-get install -y ninja-build

RUN git clone https://gitlab.freedesktop.org/mesa/mesa.git
RUN cd mesa

RUN cp /etc/apt/sources.list /etc/apt/sources.list~
RUN sed -Ei 's/^# deb-src /deb-src /' /etc/apt/sources.list
RUN apt-get update

RUN apt-get build-dep mesa -y
RUN apt-get install ninja-build -y

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]
