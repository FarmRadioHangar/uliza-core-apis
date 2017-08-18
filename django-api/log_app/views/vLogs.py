from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Log
from log_app.serializers import LogSerializer

class LogGet(generics.ListCreateAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    filter_fields = ['id']

class LogEntity(generics.RetrieveUpdateAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    lookup_field = 'id'