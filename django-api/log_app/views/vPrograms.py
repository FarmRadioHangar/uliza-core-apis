from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Program
from log_app.serializers import ProgramSerializer

class ProgramGet(generics.ListCreateAPIView):

    queryset = Program.objects.all()
    model = Program
    serializer_class = ProgramSerializer
    filter_fields = ['id','radio_station']

class ProgramEntity(generics.RetrieveUpdateAPIView):

    queryset = Program.objects.all()
    model = Program
    serializer_class = ProgramSerializer
    lookup_field = 'id'