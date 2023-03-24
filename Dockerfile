FROM rust:latest

COPY . /tmp/rambler
WORKDIR /tmp/rambler
RUN cargo install --path . --root /

USER rambler
ENTRYPOINT ["rambler", "-d", "$RAMBLER_DATA", "bot", "-k", "$RAMBLER_KEY", "-r", "$RAMBLER_ROOM"]
