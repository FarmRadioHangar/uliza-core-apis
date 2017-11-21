from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Project 
from log_app.serializers import ProjectSerializer

import django_filters
from rest_framework import filters

class ProjectFilter(filters.FilterSet):
	end_date__gte = django_filters.DateTimeFilter(name="end_date", lookup_expr='gte')
	start_date__lte = django_filters.DateTimeFilter(name="start_date", lookup_expr='lte')

	class Meta:
		model = Project
		fields = ['id','country','end_date__gte','start_date__lte']
		

class ProjectGet(generics.ListCreateAPIView):

	queryset = Project.objects.all()
	model = Project 
	serializer_class = ProjectSerializer
	filter_class = ProjectFilter

class ProjectEntity(generics.RetrieveUpdateAPIView):
	queryset = Project.objects.all()
	model = Project 
	serializer_class = ProjectSerializer
	lookup_field = 'id'