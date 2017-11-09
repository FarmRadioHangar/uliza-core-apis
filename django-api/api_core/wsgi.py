"""
WSGI config for api_core project.

It exposes the WSGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/1.8/howto/deployment/wsgi/
"""

import os,sys

sys.path.append("/home/ubuntu/uliza-api-cores/django-api")
sys.path.append("/home/ubuntu/uliza-api-cores/django-api/api_core")

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "api_core.settings")

from django.core.wsgi import get_wsgi_application
application = get_wsgi_application()
