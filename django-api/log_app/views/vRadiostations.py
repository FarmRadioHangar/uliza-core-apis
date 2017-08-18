from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import RadioStation
from log_app.serializers import RadioStationSerializer

class RadioStationGet(generics.ListCreateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    filter_fields = ['id','country']

class RadioStationEntity(generics.RetrieveUpdateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    lookup_field = 'id'