from django.conf.urls import include, url
from uliza.views.Participants import (
        Participants,
        ParticipantsInstance)
from uliza.views.RegistrationCalls import (
        RegistrationCalls,
        RegistrationCallsMissingLocation,
        RegistrationCallsInstance,
        RegistrationCallsVotoInstance,
        RegistrationCallsParticipant)
from uliza.views.APIRoot import APIRoot

participants = [
    url(r'^(?P<participant>\d+)/registration_call/$',
        RegistrationCallsParticipant.as_view()),
    url(r'^(?P<id>\d+)/$', ParticipantsInstance.as_view()),
    url(r'^$', Participants.as_view()),
]

registration_calls = [
    url(r'^(?P<id>\d+)/$', RegistrationCallsInstance.as_view()),
    url(r'^missing_location/$', RegistrationCallsMissingLocation.as_view()),
    url(r'^voto_call/(?P<voto_call_id>\d+)/$',
        RegistrationCallsVotoInstance.as_view()),
    url(r'^$', RegistrationCalls.as_view()),
]

urlpatterns = [
    url(r'^$', APIRoot.as_view()),
    url(r'^participants/', include(participants, 'participants')),
    url(r'^registration_calls/', include(registration_calls,
        'registration_calls'))
]
