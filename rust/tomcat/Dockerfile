# uaz_rust_agent
FROM rust:1.65

# install Ubuntu tools
run apt update \
    && apt install -y \
        cmake openssl libclang-13-dev

# copy source files
COPY . .

run cargo build --release

