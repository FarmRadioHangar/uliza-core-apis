from django.conf.urls import patterns, include, url
from covid.views import *

urlpatterns = patterns('',
    url('covid19/chat_users$',ChatUserGet.as_view()),
    url('covid19/chat_users/(?P<id>\d+)$',ChatUserEntity.as_view()),
    url('covid19/questions$',QuestionGet.as_view()),
    url('covid19/questions/(?P<id>\d+)$',QuestionEntity.as_view()),
    url('covid19/contents$',ContentGet.as_view()),
    url('covid19/topic/(?P<lang>\w+)/(?P<topic>\w+)$','covid.views.content',name='covid_content'),
)
