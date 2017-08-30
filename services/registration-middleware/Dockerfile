FROM haskell:8

COPY . /home/

WORKDIR /home

RUN stack build 

CMD exec stack exec simple-exe

EXPOSE 3034
