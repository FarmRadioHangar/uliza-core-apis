from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import MapRequest
from log_app.serializers import MapRequestSerializer
from rest_framework import filters
from django_filters.rest_framework import DjangoFilterBackend

class MapRequestGet(generics.ListCreateAPIView):

    queryset = MapRequest.objects.all()
    model = MapRequest
    serializer_class = MapRequestSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    filter_fields = ['id','emitters']
    ordering_fields = ('id')

class MapRequestEntity(generics.RetrieveUpdateAPIView):
    queryset = MapRequest.objects.all()
    model = MapRequest
    serializer_class = MapRequestSerializer
    lookup_field = 'id'
