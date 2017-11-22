from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Country
from log_app.serializers import CountrySerializer

class CountryGet(generics.ListCreateAPIView):

    queryset = Country.objects.all()
    model = Country
    serializer_class = CountrySerializer
    filter_fields = ['id','country_code']

class CountryEntity(generics.RetrieveUpdateAPIView):

    queryset = Country.objects.all()
    model = Country
    serializer_class = CountrySerializer
    lookup_field = 'id'
