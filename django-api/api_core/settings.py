"""
Django settings for api_core project.

Generated by 'django-admin startproject' using Django 1.8.

For more information on this file, see
https://docs.djangoproject.com/en/1.8/topics/settings/

For the full list of settings and their values, see
https://docs.djangoproject.com/en/1.8/ref/settings/
"""

from envparse import env

# Build paths inside the project like this: os.path.join(BASE_DIR, ...)
import os

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

env.read_envfile()

# Quick-start development settings - unsuitable for production
# See https://docs.djangoproject.com/en/1.8/howto/deployment/checklist/

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = '^+7=!#%326^+lvo^89%gh2kde^zs35^o7&xxyc$boif18mxsc6'

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = env.bool('DEBUG',default=False)

ALLOWED_HOSTS = ["*"]

# Application definition

INSTALLED_APPS = (
    'django.contrib.admin',
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.messages',
    'django.contrib.staticfiles',
    'django.contrib.sites',
    'django_filters',
    'corsheaders',
    'eav',
    'rest_framework',
    'uliza',
    'log_app',
    'covid',
    'shell_plus',
    'django_telegrambot',
    'telegram_bot'
)
MIDDLEWARE_CLASSES = (
    'django.contrib.sessions.middleware.SessionMiddleware',
    'corsheaders.middleware.CorsMiddleware',
    'django.middleware.common.CommonMiddleware',
    # 'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.auth.middleware.SessionAuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    #'django.middleware.clickjacking.XFrameOptionsMiddleware',
    'django.middleware.security.SecurityMiddleware',
)

ROOT_URLCONF = 'api_core.urls'

TEMPLATES = [
    {
        'BACKEND': 'django.template.backends.django.DjangoTemplates',
        'DIRS': [os.path.join(BASE_DIR, 'uliza', 'templates')],
        'APP_DIRS': True,
        'OPTIONS': {
            'context_processors': [
                'django.template.context_processors.debug',
                'django.template.context_processors.request',
                'django.contrib.auth.context_processors.auth',
                'django.contrib.messages.context_processors.messages',
            ],
        },
    },
]

WSGI_APPLICATION = 'api_core.wsgi.application'


# Database
# https://docs.djangoproject.com/en/1.8/ref/settings/#databases
DATABASES = {
    'default': {
        'ENGINE': env('DB_ENGINE'),
        'NAME': env('DB_NAME'),
        'USER': env('DB_USER'),
        'PASSWORD': env('DB_PASSWORD'),
        'HOST': env('DB_SERVICE_HOST'),
        'PORT': env('DB_SERVICE_PORT'),
        'OPTIONS': {'charset': 'utf8mb4',
                    'use_unicode': True }
    },
}

GOOGLE_DRIVE_STORAGE = {
    'service_account': {
        'email': (env('GDRIVE_EMAIL')),
        'private_key_file_path': env('GDRIVE_PRIVATE_KEY')
    }
}

CORS_ORIGIN_ALLOW_ALL = True
CORS_ALLOW_HEADERS = (
    'accept',
    'accept-encoding',
    'authorization',
    'content-type',
    'content-range',
    'content-disposition',
    'dnt',
    'origin',
    'user-agent',
    'x-csrftoken',
    'x-requested-with',
)

SUB_SITE = ''
STATIC_URL = '/public/'
STATIC_ROOT = 'public'
MEDIA_ROOT = env('MEDIA_ROOT')
MEDIA_URL = '/media/'

# Internationalization
# https://docs.djangoproject.com/en/1.8/topics/i18n/

SILKY_PYTHON_PROFILER = True

LANGUAGE_CODE = 'en-us'

TIME_ZONE = 'UTC'

USE_I18N = True

USE_L10N = True

USE_TZ = True

SITE_ID = 1


DEFAULT_PODCAST_IMAGE=env('DEFAULT_PODCAST_IMAGE')

REST_FRAMEWORK = {
    'DEFAULT_FILTER_BACKENDS': (
        'django_filters.rest_framework.DjangoFilterBackend',
    ),
    'DEFAULT_RENDERER_CLASSES': (
        'rest_framework.renderers.JSONRenderer',
        'rest_framework.renderers.BrowsableAPIRenderer',
    )
}


# if DEBUG:
#     from api_core.settings_dev import *
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'handlers': {
        'file': {
            'level': 'DEBUG',
            'class': 'logging.FileHandler',
            'filename': 'debug.log',
        },
    },
    'loggers': {
        'django': {
            'handlers': ['file'],
            'level': 'DEBUG',
            'propagate': True,
        },
    },
}


if not DEBUG:
    SECURE_PROXY_SSL_HEADER = ('HTTP_X_FORWARDED_PROTO', 'https')
    SECURE_SSL_REDIRECT = True
    SESSION_COOKIE_SECURE = True
    CSRF_COOKIE_SECURE = True

    from log_app.storage.gd_storage import GoogleDriveStorage
    GDRIVE_STORAGE = GoogleDriveStorage()
else:
    GDRIVE_STORAGE = False

TELEGRAM_TOKENS = env('TELEGRAM_TOKENS',cast=list)
TELEGRAM_WEBHOOK_SITE = env('TELEGRAM_WEBHOOK_SITE')
TELEGRAM_WEBHOOK_PREFIX = env('TELEGRAM_WEBHOOK_PREFIX')

from telegram_bot.bot_settings import *
