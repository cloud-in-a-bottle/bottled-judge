FROM debian:trixie-slim
ENV RUSTUP_HOME=/opt/rust CARGO_HOME=/opt/rust PATH=/opt/rust/bin:$PATH
RUN apt-get update && apt-get install -y --no-install-recommends gcc g++ libc6-dev python3 ocaml curl ca-certificates \
 && curl -sSf https://sh.rustup.rs | sh -s -- -y -q --profile minimal --default-toolchain 1.100.0 --no-modify-path \
 && rm -rf /var/lib/apt/lists/*
CMD ["python3", "-m", "http.server", "8080", "-d", "/srv"]
