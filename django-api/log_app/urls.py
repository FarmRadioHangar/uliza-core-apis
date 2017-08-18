from django.conf.urls import patterns, include, url
from log_app.views.vRadiostations import *

radio_stations = patterns('log_app.views.vRadiostations',
    url(r'/(?P<id>\d+)$', RadioStationEntity.as_view()),
    url(r'$', RadioStations.as_view()),
)

urlpatterns = patterns('',
    url(r'radio_stations', include(radio_stations, 'radio_stations')),
)
