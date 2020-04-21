from django.shortcuts import render
from covid.models import *
from covid.serializers import *
from django.http import HttpResponse

from rest_framework import generics


class ContentGet(generics.ListCreateAPIView):
    queryset = Content.objects.all()
    model = Content
    serializer_class = ContentSerializer
    filter_fields = ['id','topic_en']

class ContentEntity(generics.RetrieveUpdateAPIView):

    queryset = Content.objects.all()
    model = Content
    serializer_class = ContentSerializer
    lookup_field = 'id'


class ChatUserGet(generics.ListCreateAPIView):
    queryset = ChatUser.objects.all()
    model = ChatUser
    serializer_class = ChatUserSerializer
    filter_fields = ['id','user_id']


class ChatUserEntity(generics.RetrieveUpdateAPIView):
    queryset = ChatUser.objects.all()
    model = ChatUser
    serializer_class = ChatUserSerializer
    lookup_field = 'id'

class QuestionGet(generics.ListCreateAPIView):
    queryset = Question.objects.all()
    model = Question
    serializer_class = QuestionSerializer
    filter_fields = ['id','content']

    def perform_create(self, serializer):
        chat_user = ChatUser.objects.get(user_id=self.request._data['user_id'])
        chat_user.full_name = self.request._data['full_name']
        chat_user.radio_station = self.request._data['radio_station']
        chat_user.save()
        serializer.save(chat_user = chat_user)




class QuestionEntity(generics.RetrieveUpdateAPIView):
    queryset = Question.objects.all()
    model = Question
    serializer_class = QuestionSerializer
    lookup_field = 'id'


def content(request,topic,lang):
    content = Content.objects.filter(topic_en=topic)

    if len(content)==0:
        return HttpResponse('Content error')

    content = content[0]
    topic = getattr(content,'topic_'+lang)
    content = getattr(content,'content_'+lang)

    return render(request, 'bot_content.html', {'topic':topic,'content':content,'lang':lang})
