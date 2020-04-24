from django.conf.urls import patterns, include, url

urlpatterns = patterns('',
    url('$','telegram_bot.views.index',name='telegram_index'),
)
