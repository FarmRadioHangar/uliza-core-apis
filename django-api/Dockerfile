FROM python:2.7.12
ENV LANG C.UTF-8

RUN mkdir /django

RUN apt-get -y update && \
    apt-get install -y \
    python \
    python-dev \
    python-pip \
    python-psycopg2 \
    mysql-client 

ADD requirements.txt /django/requirements.txt
ADD api_core /django/api_core
ADD log_app /django/log_app
ADD uliza /django/uliza
ADD manage.py /django/manage.py

RUN pip install --timeout 2000 -r /django/requirements.txt 

RUN apt-get -y update && apt-get -y autoremove
WORKDIR /django

EXPOSE 8000

CMD gunicorn -b :8000 django.wsgi
