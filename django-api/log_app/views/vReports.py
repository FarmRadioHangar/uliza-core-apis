from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
import django_filters
from rest_framework import filters

from log_app.models import Report
from log_app.serializers import ReportSerializer
from django.http import HttpResponse
import datetime,math

class ReportFilter(filters.FilterSet):
    report_date__gte = django_filters.DateFilter(name='report_date', lookup_expr='gte')
    report_date__lte = django_filters.DateFilter(name='report_date', lookup_expr='lte')

    class Meta:
        model= Report
        fields = ['id','target','report_date__gte','report_date__lte']

class ReportGet(generics.ListCreateAPIView):
    queryset = Report.objects.all()
    model = Report
    serializer_class = ReportSerializer
    filter_class = ReportFilter

class ReportEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Report.objects.all()
    model = Report
    serializer_class = ReportSerializer
    lookup_field = 'id'
