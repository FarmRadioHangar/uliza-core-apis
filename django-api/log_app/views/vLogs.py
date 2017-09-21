from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Log
from log_app.serializers import LogSerializer

from rest_framework import filters
from rest_framework.pagination import PageNumberPagination
from django_filters.rest_framework import DjangoFilterBackend

import django_filters
from rest_framework import filters

class LargeResultsSetPagination(PageNumberPagination):
	page_size = 1000
	page_size_query_param = 'page_size'
	max_page_size = 10000

class LogFilter(filters.FilterSet):
	week__lte = django_filters.DateTimeFilter(name="week", lookup_expr='lte')
	id__lt = django_filters.DateTimeFilter(name="id", lookup_expr='lt')

	class Meta:
		model = Log
		fields = ['id','program','program__radio_station__country','postpone','week__lte','id__lt']

class LogGet(generics.ListCreateAPIView):

	queryset = Log.objects.all()

	model = Log
	serializer_class = LogSerializer
	filter_class = LogFilter
	
	pagination_class = LargeResultsSetPagination
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id','created_at')
	fields = ['id']

	def get_queryset(self):
		"""
		This view should return a list of all the purchases
		for the currently authenticated user.
		"""

		queryset = Log.objects.all().select_related('program__project__id','program','program__radio_station__country__id','program__radio_station__name','program__radio_station__id','program__start_date')

		pk_list = self.request.GET.get('program__in')
		if pk_list:
			pk_list = pk_list.split(',')
			logs = queryset.filter(program__in=pk_list)
		else:
			logs = queryset

		pk_list = self.request.GET.get('project__in')
		if pk_list:
			pk_list = pk_list.split(',')
			logs = queryset.filter(program__project__in=pk_list)
		else:
			logs = queryset


		return logs


class LogEntity(generics.RetrieveUpdateAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    lookup_field = 'id'