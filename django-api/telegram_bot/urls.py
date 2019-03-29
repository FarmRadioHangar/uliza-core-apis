from django.conf.urls import patterns, include, url


urlpatterns = patterns('',
    url(r'start$','telegram_bot.views.start',name='authenticate'),
)
