FROM haskell:7.8

RUN apt-get -qq update && apt-get -qq install -y libpq-dev

COPY . /home/

WORKDIR /home

RUN stack build 

CMD exec stack exec simple-exe

EXPOSE 3034
