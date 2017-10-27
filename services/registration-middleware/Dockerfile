FROM haskell:8

COPY . /home/

WORKDIR /home

RUN apt-get update && \
    apt-get -y install libpq5 libpq-dev && \
    cabal sandbox init && \
    cabal update && \
    cabal install --only-dependencies --force-reinstalls && \
    cabal build

CMD exec dist/build/simple-exe/simple-exe

EXPOSE 3034
