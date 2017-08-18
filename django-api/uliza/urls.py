from django.conf.urls import patterns, include, url
from rest_framework.urlpatterns import format_suffix_patterns
from uliza import views
from uliza.views.Participants import *
from uliza.views.RegistrationCalls import *
from uliza.views.VotoResponseData import *

participants = patterns('uliza.views.Participants',
    url(r'/(?P<id>\d+)$', ParticipantsInstance.as_view()),
    url(r'$', Participants.as_view()),
)

registration_calls = patterns('uliza.views.RegistrationCalls',
    url(r'/(?P<id>\d+)$', RegistrationCallsInstance.as_view()),
    url(r'$', RegistrationCalls.as_view()),
)

voto_response_data = patterns('uliza.views.VotoResponseData',
    url(r'/(?P<id>\d+)$', VotoResponseDataInstance.as_view()),
    url(r'$', VotoResponseDataDefault.as_view()),
) 

urlpatterns = patterns('',
    url(r'participants', include(participants, 'participants')),
    url(r'registration_calls', include(registration_calls, 'registration_calls')),
    url(r'voto_response_data', include(voto_response_data, 'voto_response_data')),
)
