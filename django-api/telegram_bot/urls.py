from django.conf.urls import patterns, include, url
from api_core.settings import TELEGRAM_TOKEN

urlpatterns = patterns('',
    url('$','telegram_bot.views.index',name='telegram_index'),
)
