from django.http import JsonResponse
from rest_framework.decorators import api_view
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Program,Log
from log_app.serializers import ProgramSerializer,ProgramRecordingSerializer

import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework.pagination import PageNumberPagination
from rest_framework import filters
from django.http import HttpResponse
from rest_framework.response import Response
from django.shortcuts import redirect
class ProgramFilter(filters.FilterSet):
	# ids = django_filters.NumberFilter(name="pk", lookup_expr='in')
	end_date__gte = django_filters.DateTimeFilter(name="end_date", lookup_expr='gte')
	end_date__gt = django_filters.DateTimeFilter(name="end_date", lookup_expr='gt')
	end_date__lt = django_filters.DateTimeFilter(name="end_date", lookup_expr='lt')
	start_date__gte = django_filters.DateTimeFilter(name="start_date", lookup_expr='gte')
	start_date__lt = django_filters.DateTimeFilter(name="start_date", lookup_expr='lt')
	project__end_date__gte = django_filters.DateTimeFilter(name="project__end_date", lookup_expr='gte')
	country__not = django_filters.NumberFilter(name="radio_station__country", exclude=True)


	class Meta:
		model = Program
		fields = ['id','radio_station','end_date','start_date','radio_station__country','radio_station__country__name', 'project', 'access',
				  'end_date__lt','end_date__gte','start_date__gte','project__end_date__gte','end_date__gt','start_date__lt','media_backup_status']

class LargeResultsSetPagination(PageNumberPagination):
	page_size = 1000
	page_size_query_param = 'page_size'
	max_page_size = 10000


class ProgramGet(generics.ListCreateAPIView):

	# queryset = Program.objects.all().order_by('-end_date')
	model = Program
	serializer_class = ProgramSerializer
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id','created_at','end_date','radio_station__country')
	filter_class = ProgramFilter
	pagination_class = LargeResultsSetPagination

	def get_queryset(self):
		"""
		This view should return a list of all the purchases
		for the currently authenticated user.
		"""
		pk_list = self.request.GET.get('ids')
		project_search = self.request.GET.get('project__search')
		if pk_list:
			pk_list = pk_list.split(',')
			queryset = Program.objects.filter(pk__in =pk_list).order_by('-end_date').select_related('project__id','radio_station__country','radio_station__country__name','radio_station__name',).prefetch_related('access')
		elif project_search:
			from django.db.models import Q
			queryset = Program.objects.filter(Q(project__name__icontains=project_search)|Q(project__doner__icontains=project_search))
		else:
			queryset = Program.objects.all().order_by('-end_date').select_related('project__id','radio_station__country','radio_station__country__name','radio_station__name',).prefetch_related('access')

		return queryset

	def perform_create(self, serializer):
		# Set end_date by looking at the number weeks and adding it to the start_date
		from datetime  import timedelta
		from django.utils.dateparse import parse_datetime


		weeks = timedelta(weeks = int(self.request.POST['weeks'])-1)
		end_date = parse_datetime(self.request.POST['start_date']) + weeks
		serializer.save(end_date=end_date.strftime('%Y-%m-%d'))

class ProgramEntity(generics.RetrieveUpdateAPIView):
	queryset = Program.objects.all()
	model = Program
	serializer_class = ProgramSerializer
	lookup_field = 'id'

	def perform_update(self, serializer):
		# Set end_date by looking at the number weeks and adding it to the start_date
		from datetime  import timedelta
		from django.utils.dateparse import parse_datetime

		weeks = timedelta(weeks = int(self.request._data['weeks'])-1)
		end_date = parse_datetime(self.request._data['start_date']) + weeks
		serializer.save(end_date=end_date.strftime('%Y-%m-%d'))

@api_view(['GET'])
def to_archive(request):
	from django.db.models import Sum,Case,IntegerField,When,Value
	from datetime import date
	paginator = PageNumberPagination()

	if 'page_size' in request.GET:
		paginator.page_size = request.GET['page_size']
	else:
		paginator.page_size = 4

	if 'country' in request.GET:
		programs = Program.objects.filter(end_date__lt=date.today(),radio_station__country__id=request.GET['country']).annotate(recordings=Sum(Case(When(log=None, then=Value(0)),When(log__recording_backup='', then=Value(0)),default=Value(1),output_field=IntegerField()))).filter(recordings__gt=0)
	else:
		programs = Program.objects.filter(end_date__lt=date.today()).annotate(recordings=Sum(Case(When(log=None, then=Value(0)),When(log__recording_backup='', then=Value(0)),default=Value(1),output_field=IntegerField()))).filter(recordings__gt=0)

	programs= paginator.paginate_queryset(programs,request)
	programs= ProgramRecordingSerializer(programs,many=True)

	return paginator.get_paginated_response(programs.data)


def download_media_zipped(request,id):
    # zip
	from zipfile import ZipFile
	from api_core.settings import MEDIA_ROOT

	program = Program.objects.get(id=id)

	logs = Log.objects.filter(program=program).exclude(star_audio=False,recording_backup='')

	filename = 'Uliza-log-'+program.name.encode('utf-8')
	try:
		filename = filename.encode('ascii')
		filename = filename+'.zip'
	except:
		filename = 'Uliza-log-PID'+str(program.id)+'.zip'

	with ZipFile(MEDIA_ROOT+'/'+filename,'w') as zipper:
		for log in logs:
			zipper.write(log.recording_backup.path, log.recording_backup.name)

	program.media_backup_status = 'zip'
	program.save()

	if 'redirect' in request.GET:
		from api_core.settings import MEDIA_URL
		return redirect('http://'+request.META['HTTP_HOST']+MEDIA_URL+filename)

	zipper = open(MEDIA_ROOT+'/'+filename,'r')
	response = HttpResponse(zipper,content_type='application/zip')
	response['Content-Disposition'] = 'attachment; filename='+filename

	return response

def delete_all_media(request,id):
	from api_core.settings import MEDIA_ROOT
	import os

	program = Program.objects.get(id=id)
	logs = Log.objects.filter(program=program).exclude(star_audio=False,recording_backup='')

	for log in logs:
		os.unlink(log.recording_backup.path)
		log.recording_backup = None
		log.save()

	if program.media_backup_status == 'zip':
		filename = 'Uliza-log-'+program.name.encode('utf-8')
		try:
			filename = filename.encode('ascii')
			filename = filename+'.zip'
		except:
			filename = 'Uliza-log-PID'+str(program.id)+'.zip'


		os.unlink(MEDIA_ROOT+'/'+filename)

		program.media_backup_status = 'removed'
		program.save()

	return HttpResponse('OK')
