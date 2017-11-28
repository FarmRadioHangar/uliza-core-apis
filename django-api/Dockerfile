FROM python:2.7.12
ENV LANG C.UTF-8

RUN mkdir /django

RUN apt-get -y update && apt-get install -y mysql-client 

ADD requirements.txt /django/requirements.txt
ADD api_core /django/api_core
ADD log_app /django/log_app
ADD uliza /django/uliza
ADD django-eav /django/django-eav
ADD manage.py /django/manage.py

WORKDIR /django

RUN pip install --upgrade pip && pip install -r requirements.txt 

EXPOSE 8000

CMD python manage.py runserver 0.0.0.0:8000
