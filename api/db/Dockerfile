FROM postgres:9.6

ADD . /home/db
ADD dbup.sh /docker-entrypoint-initdb.d/

RUN rm /bin/sh && ln -s /bin/bash /bin/sh

# Update the repository sources list and install dependencies
RUN apt-get update \
 && apt-get install -y curl apt-utils \
 && apt-get -y autoclean

ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 6.0.0

RUN curl --silent -o- \
    https://raw.githubusercontent.com/creationix/nvm/v0.31.2/install.sh | bash

# Install node and npm
RUN source $NVM_DIR/nvm.sh \
 && nvm install $NODE_VERSION \
 && nvm alias default $NODE_VERSION \
 && nvm use default

# Add node and npm to $PATH 
ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH $NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH

# Confirm installation
RUN node -v
RUN npm -v

RUN cd /home/db && npm install
