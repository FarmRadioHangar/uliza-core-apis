from django.conf.urls import include, url
from django.contrib import admin
from rest_framework.documentation import include_docs_urls
from settings import TELEGRAM_TOKEN

urlpatterns = [
    url(r'^admin', include(admin.site.urls)),
    url(r'^api/v1/', include('log_app.urls')),
    url(r'^api/v1/', include('uliza.urls')),
    url(r'^api/docs/', include_docs_urls(title='Uliza API')),
    url(r'^', include('django_telegrambot.urls')),
    # url(r'^api/v1/telegram/', include('telegram_bot.urls')),
]
