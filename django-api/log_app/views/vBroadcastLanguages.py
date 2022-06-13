from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import BroadcastLanguage
from log_app.serializers import BroadcastLanguageSerializer
import django_filters
from rest_framework import filters

class BroadcastLanguageGet(generics.ListCreateAPIView):
    queryset = BroadcastLanguage.objects.all()
    model = BroadcastLanguage
    serializer_class = BroadcastLanguageSerializer
    filter_fields = ['id']

class BroadcastLanguageEntity(generics.RetrieveUpdateAPIView):
    queryset = BroadcastLanguage.objects.all()
    model = BroadcastLanguage
    serializer_class = BroadcastLanguageSerializer
    lookup_field = 'id'
