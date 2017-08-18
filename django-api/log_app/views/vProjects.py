from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Project 
from log_app.serializers import ProjectSerializer

class ProjectGet(generics.ListCreateAPIView):

    queryset = Project.objects.all()
    model = Project 
    serializer_class = ProjectSerializer
    filter_fields = ['id']

class ProjectEntity(generics.RetrieveUpdateAPIView):
    queryset = Project.objects.all()
    model = Project 
    serializer_class = ProjectSerializer
    lookup_field = 'id'