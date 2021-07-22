from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Country
from log_app.serializers import CountrySerializer
from rest_framework import filters
from django_filters.rest_framework import DjangoFilterBackend

class CountryGet(generics.ListCreateAPIView):

    queryset = Country.objects.all()
    model = Country
    serializer_class = CountrySerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    filter_fields = ['id','country_code','exclude']
    ordering_fields = ('id','exclude')

class CountryEntity(generics.RetrieveUpdateAPIView):

    queryset = Country.objects.all()
    model = Country
    serializer_class = CountrySerializer
    lookup_field = 'id'
