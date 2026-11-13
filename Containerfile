FROM debian:trixie-slim
RUN apt-get update && apt-get install -y --no-install-recommends gcc g++ libc6-dev python3 ocaml ocaml-dune rustc lighttpd util-linux procps
RUN useradd -u 1000 -M web-server && useradd -u 20000 -M run
COPY . /src
RUN cd /src && dune build --release && cp _build/install/default/bin/judge /usr/local/bin/judge && cp _build/install/default/bin/web /srv/web.cgi
CMD ["judge"]
