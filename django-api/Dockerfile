FROM python:2.7.12
ENV LANG C.UTF-8

RUN mkdir /django

WORKDIR /django

ADD docker/requirements.txt /django/requirements.txt
ADD docker/django-eav /django/django-eav

RUN virtualenv env && \
    . env/bin/activate && \
    pip install --upgrade pip && \
    pip install -r requirements.txt && \
    django-admin startproject core_api . && \
    python manage.py startapp uliza

ADD docker /django
ADD uliza /django/uliza

EXPOSE 8000

CMD . env/bin/activate && python manage.py runserver 0.0.0.0:8000
