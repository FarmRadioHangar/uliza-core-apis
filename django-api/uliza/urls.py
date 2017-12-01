from django.conf.urls import include, url
from uliza.views.Participants import (
        Participants,
        ParticipantsInstance)
from uliza.views.RegistrationCalls import (
        RegistrationCalls,
        RegistrationCallsInstance)
from uliza.views.VotoWebhookLog import (
        VotoWebhookLogCollection,
        VotoWebhookLogInstance)
from uliza.views.VotoSurveyRegistrationTree import (
        VotoSurveyRegistrationTreeCollection,
        VotoSurveyRegistrationTreeInstance)
from uliza.views.APIRoot import APIRoot

participants = [
    url(r'^(?P<id>\d+)/$', ParticipantsInstance.as_view()),
    url(r'^$', Participants.as_view()),
]

registration_calls = [
    url(r'^(?P<id>\d+)/$', RegistrationCallsInstance.as_view()),
    url(r'^$', RegistrationCalls.as_view()),
]

voto_webhook_log = [
    url(r'^(?P<id>\d+)/$', VotoWebhookLogInstance.as_view()),
    url(r'^$', VotoWebhookLogCollection.as_view()),
]

voto_survey_registration_tree = [
    url(r'^(?P<voto_survey_id>\d+)/$',
        VotoSurveyRegistrationTreeInstance.as_view()),
    url(r'^$', VotoSurveyRegistrationTreeCollection.as_view()),
]

urlpatterns = [
    url(r'^$', APIRoot.as_view()),
    url(r'^participants/', include(participants, 'participants')),
    url(r'^registration_calls/', include(registration_calls,
        'registration_calls')),
    url(r'^voto_webhook_log/', include(voto_webhook_log, 'voto_webhook_log')),
    url(r'^voto_survey_registration_tree/', include(
        voto_survey_registration_tree,
        'voto_survey_registration_tree'
    ))
]
