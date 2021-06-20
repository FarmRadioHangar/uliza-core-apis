from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import RadioType
from log_app.serializers import RadioTypeSerializer
import django_filters
from rest_framework import filters

class RadioTypeGet(generics.ListCreateAPIView):
    queryset = RadioType.objects.all()
    model = RadioType
    serializer_class = RadioTypeSerializer
    filter_fields = ['id']

    def get_queryset(self):
        pk_list = self.request.GET.get('ids')
        if pk_list:
            pk_list = pk_list.split(',')
            queryset = RadioType.objects.filter(pk__in =pk_list)
        else:
            queryset = RadioType.objects.all()

        return queryset

class RadioTypeEntity(generics.RetrieveUpdateAPIView):
    queryset = RadioType.objects.all()
    model = RadioType
    serializer_class = RadioTypeSerializer
    lookup_field = 'id'
