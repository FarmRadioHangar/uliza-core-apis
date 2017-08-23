from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Program
from log_app.serializers import ProgramSerializer

import django_filters
from rest_framework import filters

class ProgramFilter(filters.FilterSet):
    end_date__gte = django_filters.DateTimeFilter(name="end_date", lookup_expr='gte')
    end_date__lt = django_filters.DateTimeFilter(name="end_date", lookup_expr='lt')
    start_date__gte = django_filters.DateTimeFilter(name="start_date", lookup_expr='gte')

    class Meta:
        model = Program
        fields = ['id', 'radio_station','end_date','start_date','radio_station__country','end_date__lt','end_date__gte','start_date__gte']


class ProgramGet(generics.ListCreateAPIView):

    queryset = Program.objects.all().order_by('-end_date')
    model = Program
    serializer_class = ProgramSerializer
    filter_class = ProgramFilter

class ProgramEntity(generics.RetrieveUpdateAPIView):

    queryset = Program.objects.all()
    model = Program
    serializer_class = ProgramSerializer
    lookup_field = 'id'