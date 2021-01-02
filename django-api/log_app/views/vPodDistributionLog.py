from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import PodDistributionLog
from log_app.serializers import PodDistributionLogSerializer
from rest_framework.authentication import get_authorization_header
from django.views.decorators.http import require_POST
from django.http import HttpResponse

import requests

import django_filters
from rest_framework import filters

class PodDistributionLogGet(generics.ListCreateAPIView):
    queryset = PodDistributionLog.objects.all()
    model = PodDistributionLog
    serializer_class = PodDistributionLogSerializer
    filter_backends = (filters.OrderingFilter,filters.DjangoFilterBackend)
    filter_fields = ['id','podcast']
    ordering_fields = ('id','last_updated_at')


class PodDistributionLogEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = PodDistributionLog.objects.all()
    model = PodDistributionLog
    serializer_class = PodDistributionLogSerializer
    lookup_field = 'id'
