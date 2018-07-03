from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Format
from log_app.serializers import FormatSerializer
import django_filters
from rest_framework import filters

class FormatGet(generics.ListCreateAPIView):
    queryset = Format.objects.all()
    model = Format
    serializer_class = FormatSerializer
    filter_fields = ['id','legacy','always_checked']

    def get_queryset(self):
        pk_list = self.request.GET.get('pk_list')
        if pk_list:
            pk_list = pk_list.split(',')
            queryset = Format.objects.filter(pk__in=pk_list).order_by('name')
        else:
            queryset = Format.objects.all().order_by('name')
        return queryset


class FormatEntity(generics.RetrieveUpdateAPIView):
    queryset = Format.objects.all()
    model = Format
    serializer_class = FormatSerializer
    lookup_field = 'id'
