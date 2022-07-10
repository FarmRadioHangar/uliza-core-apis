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

class ReportGet(generics.ListCreateAPIView):
    queryset = Report.objects.all()
    model = Report
    serializer_class = ReportSerializer
    filter_fields = ['id','target']

class ReportEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Report.objects.all()
    model = Report
    serializer_class = ReportSerializer
    lookup_field = 'id'
