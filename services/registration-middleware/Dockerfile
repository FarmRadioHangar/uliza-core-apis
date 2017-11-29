FROM haskell:8

ADD app /home/app
ADD Setup.hs /home/Setup.hs
ADD simple.cabal /home/simple.cabal
ADD src /home/src
ADD test /home/test

WORKDIR /home

RUN apt-get update && \
    apt-get -y install libpq5 libpq-dev wget unzip && \
    cabal sandbox init && \
    cabal update && \
    cabal install --only-dependencies --force-reinstalls && \
    cabal build && \
    wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-amd64.zip && \
    unzip ngrok-stable-linux-amd64.zip

CMD exec dist/build/simple-exe/simple-exe

EXPOSE 3034
