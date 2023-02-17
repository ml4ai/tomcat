FROM ubuntu:20.04

# copy source files
RUN mkdir src
COPY ./src ./src
COPY . .

# install Ubuntu tools
RUN apt-get update 
RUN apt-get install -y wget
RUN apt-get install -y curl 
RUN apt-get install -y gcc 
RUN apt-get install -y make 
RUN apt-get install -y cmake 
RUN apt-get install -y build-essential
RUN apt-get install -y libssl-dev
RUN apt-get install -y --no-install-recommends pkg-config

# set up rust directory
RUN mkdir -m777 /opt/rust 
ENV RUSTUP_HOME=/opt/rust
ENV PATH=/opt/rust/bin:$PATH

# set up cargo directory
RUN mkdir -m777 /opt/cargo
ENV CARGO_HOME=/opt/cargo
ENV PATH=/opt/cargo/bin:$PATH

# install rust
RUN wget --https-only --secure-protocol=TLSv1_2 -O- https://sh.rustup.rs | sh /dev/stdin -y
RUN rustup target add x86_64-unknown-freebsd
RUN printf '#!/bin/sh\nexport CARGO_HOME=/opt/cargo\nexec /bin/sh "$@"\n' >/usr/local/bin/sh
RUN chmod +x /usr/local/bin/sh

# clean up install cache
RUN apt-get clean
