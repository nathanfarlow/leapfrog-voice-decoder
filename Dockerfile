FROM ubuntu:24.04 AS build

ENV DEBIAN_FRONTEND=noninteractive \
    OPAMYES=1 \
    OPAMWITHDOC=false \
    OPAMWITHTEST=false

RUN apt-get update && apt-get install -y \
    opam \
    build-essential \
    git \
    m4 \
    && rm -rf /var/lib/apt/lists/*

USER ubuntu
WORKDIR /home/ubuntu

RUN opam init -y --disable-sandboxing --compiler=5.4.0 && \
    opam install dune core async core_unix ppx_jane && \
    opam clean -a -c -s --logs -r && \
    rm -rf ~/.opam/download-cache && \
    echo 'eval $(opam env)' >> ~/.profile

COPY --chown=ubuntu:ubuntu . src
RUN cd src && opam exec -- dune build bin && opam exec -- dune runtest

FROM ubuntu:24.04

COPY --from=build /home/ubuntu/src/_build/default/bin/main.exe /usr/local/bin/lfc

ENTRYPOINT ["lfc"]
