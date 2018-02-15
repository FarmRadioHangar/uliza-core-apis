from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Program
from log_app.serializers import ProgramSerializer

import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework.pagination import PageNumberPagination
from rest_framework import filters

class ProgramFilter(filters.FilterSet):
	# ids = django_filters.NumberFilter(name="pk", lookup_expr='in')
	end_date__gte = django_filters.DateTimeFilter(name="end_date", lookup_expr='gte')
	end_date__gt = django_filters.DateTimeFilter(name="end_date", lookup_expr='gt')
	end_date__lt = django_filters.DateTimeFilter(name="end_date", lookup_expr='lt')
	start_date__gte = django_filters.DateTimeFilter(name="start_date", lookup_expr='gte')
	start_date__lt = django_filters.DateTimeFilter(name="start_date", lookup_expr='lt')
	project__end_date__gte = django_filters.DateTimeFilter(name="project__end_date", lookup_expr='gte')


	class Meta:
		model = Program
		fields = ['id','radio_station','end_date','start_date','radio_station__country', 'project', 'access',
				  'end_date__lt','end_date__gte','start_date__gte','project__end_date__gte','end_date__gt','start_date__lt']

class LargeResultsSetPagination(PageNumberPagination):
	page_size = 1000
	page_size_query_param = 'page_size'
	max_page_size = 10000


class ProgramGet(generics.ListCreateAPIView):

	# queryset = Program.objects.all().order_by('-end_date')
	model = Program
	serializer_class = ProgramSerializer
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id','created_at','end_date')
	filter_class = ProgramFilter
	pagination_class = LargeResultsSetPagination

	def get_queryset(self):
		"""
		This view should return a list of all the purchases
		for the currently authenticated user.
		"""
		pk_list = self.request.GET.get('ids')
		if pk_list:
			pk_list = pk_list.split(',')
			queryset = Program.objects.filter(pk__in =pk_list).order_by('-end_date').select_related('project__id','radio_station__country','radio_station__name',).prefetch_related('access')
		else:
			queryset = Program.objects.all().order_by('-end_date').select_related('project__id','radio_station__country','radio_station__name',).prefetch_related('access')

		return queryset

	def perform_create(self, serializer):
		# Set end_date by looking at the number weeks and adding it to the start_date
		from datetime  import timedelta
		from django.utils.dateparse import parse_datetime


		weeks = timedelta(weeks = int(self.request.POST['weeks'])-1)
		end_date = parse_datetime(self.request.POST['start_date']) + weeks
		print end_date.strftime('%Y-%m-%d')

		serializer.save(end_date=end_date.strftime('%Y-%m-%d'))

class ProgramEntity(generics.RetrieveUpdateAPIView):

    queryset = Program.objects.all()
    model = Program
    serializer_class = ProgramSerializer
    lookup_field = 'id'
