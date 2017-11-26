from django.conf.urls import include, url
from django.contrib import admin

urlpatterns = [
    url(r'^admin', include(admin.site.urls)),
    url(r'^api/v1/', include('log_app.urls')),
    url(r'^api/v1/', include('uliza.urls')),
]
