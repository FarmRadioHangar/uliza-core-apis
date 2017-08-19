from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from uliza.models import VotoResponseData
from uliza.serializers import VotoResponseDataSerializer

class VotoResponseDataDefault(generics.ListCreateAPIView):

    queryset = VotoResponseData.objects.all()
    model = VotoResponseData
    serializer_class = VotoResponseDataSerializer

class VotoResponseDataInstance(generics.RetrieveUpdateAPIView):

    queryset = VotoResponseData.objects.all()
    model = VotoResponseData
    serializer_class = VotoResponseDataSerializer
    lookup_field = 'id'
