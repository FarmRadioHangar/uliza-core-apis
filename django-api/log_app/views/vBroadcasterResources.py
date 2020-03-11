from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import BroadcasterResource
from log_app.serializers import BroadcasterResourceSerializer
import django_filters
from rest_framework import filters

class BroadcasterResourceGet(generics.ListCreateAPIView):
    queryset = BroadcasterResource.objects.all()
    model = BroadcasterResource
    serializer_class = BroadcasterResourceSerializer
    filter_fields = ['id']

class BroadcasterResourceEntity(generics.RetrieveUpdateAPIView):
    queryset = BroadcasterResource.objects.all()
    model = BroadcasterResource
    serializer_class = BroadcasterResourceSerializer
    lookup_field = 'id'
