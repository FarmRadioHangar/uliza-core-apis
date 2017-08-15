from django.conf.urls import include, url
# import log_app, uliza


urlpatterns = [
    url(r'^log/', include('log_app.urls')),
    # url(r'^api/uliza', include(uliza.urls)),
]
