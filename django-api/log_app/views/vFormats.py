# uncompyle6 version 3.1.2
# Python bytecode 2.7 (62211)
# Decompiled from: Python 2.7.14 (default, Mar 22 2018, 15:04:47) 
# [GCC 4.2.1 Compatible Apple LLVM 9.0.0 (clang-900.0.39.2)]
# Embedded file name: /Users/jigsa/code/uliza-core-apis/django-api/log_app/views/vFormats.py
# Compiled at: 2018-04-25 15:23:09
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
    filter_fields = ['id']

    def get_queryset(self):
        """
        This view should return a list of all the purchases
        for the currently authenticated user.
        """
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