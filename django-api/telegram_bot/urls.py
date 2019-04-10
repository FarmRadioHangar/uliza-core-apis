from django.conf.urls import patterns, include, url
from api_core.settings import TELEGRAM_TOKEN 

urlpatterns = patterns('',
    url(r'activate$','telegram_bot.views.activate',name='activate'),
    url(TELEGRAM_TOKEN+'$','telegram_bot.views.main',name='telegram'),
)
