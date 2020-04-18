from django.conf.urls import patterns, include, url
from api_core.settings import TELEGRAM_TOKEN

urlpatterns = patterns('',
    url('content$','covid.views.content',name='covid_content'),
)
