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
    python \
    pkg-config \
    cmake \
    opam \
    python3 \
    python3-distutils \
    ninja-build

RUN ln -s /usr/bin/lli-10.0 /usr/bin/lli
RUN ln -s /usr/bin/llc-10.0 /usr/bin/llc

RUN opam init --disable-sandboxing -vv
RUN opam install \
    llvm.10.0.0 \
    ocamlfind -y
RUN eval `opam config env`

##################################################################
# for building MESA
##################################################################
# add dependencies
RUN apt-get build-dep mesa -y
RUN apt-get install -y libdrm-dev libxxf86vm-dev libxt-dev xutils-dev flex bison xcb libx11-xcb-dev libxcb-glx0 libxcb-glx0-dev xorg-dev libxcb-dri2-0-dev
RUN apt-get install -y libelf-dev libunwind-dev valgrind libwayland-dev wayland-protocols libwayland-egl-backend-dev
RUN apt-get install -y libxcb-shm0-dev libxcb-dri3-dev libxcb-present-dev libxshmfence-dev

# add environment variable
RUN export PATH="/usr/bin/python:$PATH"

# add Mako and meson dependency from python
RUN wget https://bootstrap.pypa.io/get-pip.py
RUN python3 get-pip.py
RUN pip install Mako
RUN pip install meson

# download and install newest libdrm
RUN wget https://dri.freedesktop.org/libdrm/libdrm-2.4.105.tar.xz
RUN tar xf libdrm-2.4.105.tar.xz && libdrm-2.4.105.tar.xz
RUN cd libdrm-2.4.105
RUN meson build/
RUN ninja
RUN ninja install
RUN cd ~

# download mesa
RUN wget https://archive.mesa3d.org//mesa-20.3.5.tar.xz
RUN tar xf mesa-20.3.5.tar.xz && rm mesa-20.3.5.tar.xz
RUN cd mesa-20.3.5

# add things to sources.list
RUN cp /etc/apt/sources.list /etc/apt/sources.list~
RUN sed -Ei 's/^# deb-src /deb-src /' /etc/apt/sources.list
RUN apt-get update

# build, compile and install
RUN meson builddir/
RUN cd builddir
RUN ninja
RUN ninja install

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]
