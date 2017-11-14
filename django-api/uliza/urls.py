from django.conf.urls import patterns, include, url
from rest_framework.urlpatterns import format_suffix_patterns
from uliza import views
from uliza.views.Participants import *
from uliza.views.RegistrationCalls import *
from uliza.views.VotoWebhookLog import *
from uliza.views.VotoSurveyRegistrationTree import *

participants = patterns(
    'uliza.views.Participants',
    url(r'/(?P<id>\d+)$', ParticipantsInstance.as_view()),
    url(r'$', Participants.as_view()),
)

registration_calls = patterns(
    'uliza.views.RegistrationCalls',
    url(r'/(?P<id>\d+)$', RegistrationCallsInstance.as_view()),
    url(r'$', RegistrationCalls.as_view()),
)

voto_webhook_log = patterns(
    'uliza.views.VotoWebhookLog',
    url(r'/(?P<id>\d+)$', VotoWebhookLogInstance.as_view()),
    url(r'$', VotoWebhookLogDefault.as_view()),
)

voto_survey_registration_tree = patterns(
    'uliza.views.VotoSurveyRegistrationTree',
    url(r'/(?P<voto_survey_id>\d+)$', VotoSurveyRegistrationTreeInstance.as_view()),
    url(r'$', VotoSurveyRegistrationTreeDefault.as_view()),
)

urlpatterns = patterns(
    '',
    url(r'participants', include(participants, 'participants')),
    url(r'registration_calls', include(registration_calls,
        'registration_calls')),
    url(r'voto_webhook_log', include(voto_webhook_log, 'voto_webhook_log')),
    url(r'voto_survey_registration_tree', include(
        voto_survey_registration_tree,
        'voto_survey_registration_tree'
    ))
)
