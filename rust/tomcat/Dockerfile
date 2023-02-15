FROM ubuntu:20.04

RUN mkdir src
COPY ./src ./src
COPY . .

RUN apt-get update && apt-get install -y wget
RUN mkdir -m777 /opt/rust /opt/cargo
ENV RUSTUP_HOME=/opt/rust
ENV CARGO_HOME=/opt/cargo
ENV PATH=/opt/cargo/bin:$PATH
RUN wget --https-only --secure-protocol=TLSv1_2 -O- https://sh.rustup.rs | sh /dev/stdin -y
RUN apt-get install -y curl 
RUN apt-get install -y gcc 
RUN apt-get install -y make 
RUN apt-get install -y build-essential
RUN apt-get install libssl-dev
RUN rustup target add x86_64-unknown-freebsd
RUN printf '#!/bin/sh\nexport CARGO_HOME=/opt/cargo\nexec /bin/sh "$@"\n' >/usr/local/bin/sh
RUN chmod +x /usr/local/bin/sh
RUN mkdir rust_docker_image

ENTRYPOINT cargo run

