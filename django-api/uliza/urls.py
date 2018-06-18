from django.conf.urls import include, url
from rest_framework.urlpatterns import format_suffix_patterns
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
from uliza.views.events import Events
from uliza.views.answers import Answers, AnswersInstance
from uliza.views.content_manager import Role, RoleInstance, Detail, \
DetailInstance, ContactDetail, ContactDetailInstance

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

answers = [
    url(r'^(?P<id>\d+)/$', AnswersInstance.as_view(), name='question'),
    url(r'^$', Answers.as_view(), name='questions'),
]

roles = [
    url(r'^(?P<id>\d+)/$', RoleInstance.as_view()),
    url(r'^$', Role.as_view()),
]

details = [
    url(r'^(?P<id>\d+)/$', DetailInstance.as_view()),
    url(r'^$', Detail.as_view()),
]

contact_details = [
    url(r'^(?P<id>\d+)/$', ContactDetailInstance.as_view()),
    url(r'^$', ContactDetail.as_view()),
]

webhooks = [
	url(r'^$', Events.as_view())
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
    )), 
    url(r'^answers/questions/', include(answers, 'questions')),
    url(r'^hooks/viamo', include(webhooks, 'webhooks')),
    url(r'^content/roles/', include(roles, 'roles')),
    url(r'^content/details/', include(details, 'details')),
    url(r'^content/contact_details/', include(contact_details, 'contact_details')),

]
urlpatterns = format_suffix_patterns(urlpatterns)
