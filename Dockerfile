FROM alpine:3.19.1
RUN apk add binutils-gold curl gcc g++ gmp-dev libc-dev libffi-dev libpq-dev make musl-dev ncurses-dev perl tar xz zlib-dev

RUN curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

ARG GHC=9.4.8
ARG CABAL=3.10.2.1
RUN ghcup -v install ghc --isolate /usr/local --force 9.4.8 && \
    ghcup -v install cabal --isolate /usr/local/bin --force 3.10.2.1

WORKDIR /opt/majorplayer
RUN cabal update

COPY ./majorplayer.cabal /opt/majorplayer/majorplayer.cabal
RUN cabal build --only-dependencies

COPY ./app /opt/majorplayer/app/
COPY ./env /opt/majorplayer/env/
COPY ./migrations/ /opt/majorplayer/migrations/
RUN cabal build
CMD ["cabal", "run"]
