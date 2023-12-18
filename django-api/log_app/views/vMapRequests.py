from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import MapRequest,RadioStation
from log_app.serializers import MapRequestSerializer
from rest_framework import filters
import django_filters
from django_filters.rest_framework import DjangoFilterBackend

class MapRequestGet(generics.ListCreateAPIView):

    queryset = MapRequest.objects.all()
    model = MapRequest
    serializer_class = MapRequestSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    filter_fields = ['id','emitters','status']
    ordering_fields = ('id','status')

    def get_queryset(self):
        country = self.request.GET.get('country')
        if country:
            stations = RadioStation.objects.filter(country = country)
            queryset = MapRequest.objects.filter(emitters__radio_station__in=stations).order_by('-id')
        else:
            queryset = MapRequest.objects.all().order_by('-id')

        return queryset

class MapRequestEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = MapRequest.objects.all()
    model = MapRequest
    serializer_class = MapRequestSerializer
    lookup_field = 'id'
