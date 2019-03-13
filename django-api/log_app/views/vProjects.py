from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Project
from log_app.serializers import ProjectSerializer
from rest_framework.pagination import PageNumberPagination
from django_filters.rest_framework import DjangoFilterBackend

import django_filters
from rest_framework import filters

class LargeResultsSetPagination(PageNumberPagination):
	page_size = 1000
	page_size_query_param = 'page_size'
	max_page_size = 10000


class ProjectFilter(filters.FilterSet):
	end_date__gte = django_filters.DateTimeFilter(name="end_date", lookup_expr='gte')
	start_date__lte = django_filters.DateTimeFilter(name="start_date", lookup_expr='lte')
	country__not = django_filters.NumberFilter(name='country', exclude=True)

	class Meta:
		model = Project
		fields = ['id','country','end_date__gte','start_date__lte','image']


class ProjectGet(generics.ListCreateAPIView):
	queryset = Project.objects.all()
	model = Project
	serializer_class = ProjectSerializer
	ordering_fields=('id','created_at')
	filter_class = ProjectFilter
	filter_backends = (filters.OrderingFilter,DjangoFilterBackend)
	pagination_class = LargeResultsSetPagination
	fields=['id']

class ProjectEntity(generics.RetrieveUpdateAPIView):
	queryset = Project.objects.all()
	model = Project
	serializer_class = ProjectSerializer
	lookup_field = 'id'
