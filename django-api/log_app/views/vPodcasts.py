from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Podcast
from log_app.serializers import PodcastSerializer

import django_filters
from rest_framework import filters

class PodcastGet(generics.ListCreateAPIView):

    queryset = Podcast.objects.all()
    model = Podcast
    serializer_class = PodcastSerializer
    filter_fields = ['id','radio_station','spreaker_show_id']


class PodcastEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Podcast.objects.all()
    model = Podcast
    serializer_class = PodcastSerializer
    lookup_field = 'id'
