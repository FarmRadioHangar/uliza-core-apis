from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
import django_filters
from rest_framework import filters

from log_app.models import Indicator
from log_app.serializers import IndicatorSerializer
from django.http import HttpResponse
import datetime,math
from django_filters.rest_framework import DjangoFilterBackend

class IndicatorGet(generics.ListCreateAPIView):
    queryset = Indicator.objects.all()
    model = Indicator
    serializer_class = IndicatorSerializer
    ordering_fields = ('order','grouping')
    filter_fields = ['id','grouping','project_tied']
    filter_backends = (filters.OrderingFilter,DjangoFilterBackend)

    def get_queryset(self):
        pt = self.request.GET.get('pt')
        if pt == '0':
            queryset = Indicator.objects.filter(project_tied = None)
        else:
            queryset = Indicator.objects.all()

        return queryset



class IndicatorEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Indicator.objects.all()
    model = Indicator
    serializer_class = IndicatorSerializer
    lookup_field = 'id'
