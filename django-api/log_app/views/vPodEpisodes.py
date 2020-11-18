from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import PodEpisode
from log_app.serializers import PodEpisodeSerializer
from api_core.settings import SPREAKER_TOKEN
from rest_framework.authentication import get_authorization_header
from django.views.decorators.http import require_POST
from django.http import HttpResponse

import requests


import django_filters
from rest_framework import filters

class PodEpisodeGet(generics.ListCreateAPIView):
    queryset = PodEpisode.objects.all()
    model = PodEpisode
    serializer_class = PodEpisodeSerializer
    filter_backends = (filters.OrderingFilter,filters.DjangoFilterBackend)
    filter_fields = ['id','podcast','spreaker_episode_id']
    ordering_fields = ['id']


class PodEpisodeEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = PodEpisode.objects.all()
    model = PodEpisode
    serializer_class = PodEpisodeSerializer
    lookup_field = 'id'

@require_POST
def upload_to_spreaker(request, pk):
    try:
        instance = PodEpisode.objects.get(pk=pk)
    except ValueError:
        return HttpResponse("Couldn't find the episode",status="400")

    if instance.spreaker_episode_id:
        url = 'https://api.spreaker.com/v2/episodes/'+str(instance.spreaker_episode_id)
    else:
        url = 'https://api.spreaker.com/v2/shows/'+str(instance.podcast.spreaker_show_id)+'/episodes'

    title = instance.title

    if not instance.audio_file:
        return HttpResponse("Couldn't fine the audio file",status='404')

    if not title:
        title = '#'+str(instance.id)

    authorization_header =  get_authorization_header(request)
    response = requests.post(url, {'title':title},
                             files={'media_file':open(instance.audio_file.path,'rb')},
                             headers={'Authorization': authorization_header})

    import json
    content = json.loads(response.content)
    if response.status_code < 250:
        instance.spreaker_episode_id = content['response']['episode']['episode_id']
        instance.spreaker_audio_url = content['response']['episode']['media_url']
        instance.save()

    return JsonResponse(content,status=response.status_code)
