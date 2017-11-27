FROM haskell:8

ADD app /home/app
ADD Setup.hs /home/Setup.hs
ADD simple.cabal /home/simple.cabal
ADD src /home/src
ADD test /home/test

WORKDIR /home

RUN apt-get update && \
    apt-get -y install libpq5 libpq-dev && \
    cabal sandbox init && \
    cabal update && \
    cabal install --only-dependencies --force-reinstalls && \
    cabal build

CMD exec dist/build/simple-exe/simple-exe

EXPOSE 3034
