FROM rust:latest

COPY . /tmp/rambler
WORKDIR /tmp/rambler
RUN useradd rambler
RUN apt update -y && apt upgrade -y && apt install -y curl
RUN cargo install --path . --root /
RUN curl -L "https://www.dropbox.com/s/amj58y7f34fg4t5/wiki.bin?dl=1" > /tmp/wiki.bin

USER rambler
ENTRYPOINT ["rambler", "-d", "/tmp/wiki.bin", "bot"]
