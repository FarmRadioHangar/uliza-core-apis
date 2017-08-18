from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Administrator
from log_app.serializers import AdministratorSerializer

class AdministratorGet(generics.ListCreateAPIView):

    queryset = Administrator.objects.all()
    model = Administrator
    serializer_class = AdministratorSerializer
    filter_fields = ['id','country']

class AdministratorEntity(generics.RetrieveUpdateAPIView):

    queryset =Administrator.objects.all()
    model = Administrator
    serializer_class = AdministratorSerializer
    lookup_field = 'id'