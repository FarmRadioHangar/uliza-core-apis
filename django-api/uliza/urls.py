from django.conf.urls import patterns, include, url

participants = patterns('uliza.views.vParticipants',
    url(r'/(?P<id>\d+)$', 'instance', name='instance'),
    url(r'$', 'default', name='default'),
)

registration_calls = patterns('uliza.views.vRegistrationCalls',
    url(r'/(?P<id>\d+)$', 'instance', name='instance'),
    url(r'$', 'default', name='default'),
)

# eventually remove
participant_registration_status_log = patterns('uliza.views.vParticipantRegistrationStatusLog',
    url(r'/(?P<id>\d+)$', 'instance', name='instance'),
    url(r'$', 'default', name='default'),
) 

# eventually remove??
voto_response_data = patterns('uliza.views.vVotoResponseData',
    url(r'/(?P<id>\d+)$', 'instance', name='instance'),
    url(r'$', 'default', name='default'),
) 

urlpatterns = patterns('',
    url(r'participants', include(participants, 'participants')),
    url(r'registration_calls', include(registration_calls, 'registration_calls')),
    url(r'participant_registration_status_log', include(participant_registration_status_log, 'participant_registration_status_log')),
    url(r'voto_response_data', include(voto_response_data, 'voto_response_data')),
)
