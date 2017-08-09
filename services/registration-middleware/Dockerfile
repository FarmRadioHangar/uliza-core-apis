FROM samdoshi/haskell-stack:latest

COPY . /home/

WORKDIR /home

RUN stack build 

CMD exec stack exec simple-exe

EXPOSE 3034
