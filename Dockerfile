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

##################################################################
# for building MESA
##################################################################
# add environment variable
RUN export PATH="/usr/bin/python:$PATH"

# add Mako and meson dependency from python
RUN wget https://bootstrap.pypa.io/get-pip.py
RUN python3 get-pip.py
RUN pip install Mako
RUN pip install meson
RUN apt-get install libpciaccess-dev -y

# download and install newest libdrm
RUN wget https://dri.freedesktop.org/libdrm/libdrm-2.4.105.tar.xz
RUN tar xf libdrm-2.4.105.tar.xz && rm libdrm-2.4.105.tar.xz
WORKDIR libdrm-2.4.105/
RUN meson build/ && cd build && ninja && ninja install
WORKDIR ../
RUN rm -r libdrm-2.4.105/

# download mesa
RUN wget https://archive.mesa3d.org//mesa-20.3.5.tar.xz
RUN tar xf mesa-20.3.5.tar.xz && rm mesa-20.3.5.tar.xz
WORKDIR mesa-20.3.5

# add things to sources.list
RUN cp /etc/apt/sources.list /etc/apt/sources.list~
RUN sed -Ei 's/^# deb-src /deb-src /' /etc/apt/sources.list
RUN apt-get update

# add dependencies
RUN apt-get install -y libdrm-dev libxxf86vm-dev libxt-dev xutils-dev flex bison xcb libx11-xcb-dev libxcb-glx0 libxcb-glx0-dev xorg-dev libxcb-dri2-0-dev
RUN apt-get install -y libelf-dev libunwind-dev valgrind libwayland-dev wayland-protocols libwayland-egl-backend-dev
RUN apt-get install -y libxcb-shm0-dev libxcb-dri3-dev libxcb-present-dev libxshmfence-dev
RUN apt-get build-dep mesa -y

# build, compile and install
RUN meson build/ -Dosmesa=classic && ninja -C build/ && ninja -C build/ install
WORKDIR ../

# install MESA GLU
RUN git clone https://gitlab.freedesktop.org/mesa/glu.git
RUN cd glu && ./autogen.sh && ./configure --enable-osmesa --prefix=/usr/local/ && make && make install
RUN rm -r glu mesa-20.3.5 get-pip.py

# install vim for testing
# RUN apt-get install vim -y

##################################################################
# for building LLVM & others
##################################################################
RUN ln -s /usr/bin/lli-10 /usr/bin/lli
RUN ln -s /usr/bin/llc-10 /usr/bin/llc

RUN opam init --disable-sandboxing -y
RUN opam install \
    llvm.10.0.0 \
    ocamlfind \
    ocamlbuild -y
RUN eval `opam config env`

WORKDIR /root

ENTRYPOINT ["opam", "config", "exec", "--"]

CMD ["bash"]
