from django.conf.urls import include, url

urlpatterns = [
    url(r'^log/', include('log_app.urls')),
    url(r'^api/v1/', include('uliza.urls')),
]
