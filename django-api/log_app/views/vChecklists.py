from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Checklist
from log_app.serializers import ChecklistSerializer
import django_filters
from rest_framework import filters

class ChecklistGet(generics.ListCreateAPIView):
    queryset = Checklist.objects.all().order_by('-level')
    model = Checklist
    serializer_class = ChecklistSerializer
    ordering_fields = 'radio_format'
    filter_fields = ['id', 'level', 'radio_format']

    def get_queryset(self):
        """
        This view should return a list of all the purchases
        for the currently authenticated user.
        """
        pk_list = self.request.GET.get('pk_list')
        format_list = self.request.GET.get('format_list')
        if pk_list:
            pk_list = pk_list.split(',')
            queryset = Checklist.objects.filter(pk__in=pk_list)
        else:
            if format_list:
                format_list = format_list.split(',')
                queryset = Checklist.objects.filter(radio_format__in=format_list)
            else:
                queryset = Checklist.objects.all()
        return queryset


class ChecklistEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Checklist.objects.all()
    model = Checklist
    serializer_class = ChecklistSerializer
    lookup_field = 'id'
