from django.http import JsonResponse
from rest_framework.decorators import api_view
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Program,Log, PollSegment,Review, RespondentStat
from log_app.serializers import ProgramSerializer,ProgramRecordingSerializer

import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework.pagination import PageNumberPagination
from rest_framework import filters
from django.http import HttpResponse
from rest_framework.response import Response
from django.shortcuts import redirect
import datetime,math

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
		fields = ['id','radio_station','broadcast_language','end_date','start_date','radio_station__country','radio_station__country__name', 'project', 'access',
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

		if 'weeks' in self.request._data:
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
	except Exception as e:
		filename = 'Uliza-log-PID'+str(program.id)+'.zip'

	with ZipFile(MEDIA_ROOT+'/'+filename,'w',allowZip64=True) as zipper:
		try:
			for log in logs:
				zipper.write(log.recording_backup.path, log.recording_backup.name)
		except OSError: pass

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

def weeks_diff(start,end):
 monday = (start - datetime.timedelta(days=start.weekday()))
 calc = (end - monday).days
 weeks = math.floor(calc/7.0)
 remaining_days = calc%7
 return weeks,remaining_days

def stats(request):
	from django.utils.dateparse import parse_date,parse_datetime
	from django.db.models import Sum,Avg
	start_date = parse_datetime(request.GET['start_date']+' 00:00')
	end_date = parse_date(request.GET['end_date'])

	programs = Program.objects.filter(end_date__gte = start_date,start_date__lte=end_date).exclude(project__country__exclude=True)

	if not request.GET['country'] == 'all':
		programs = programs.filter(project__country = request.GET['country'])

	total_episodes = 0
	total_hours = 0
	total_responses = 0
	average_respondents = 0
	total_polls = 0
	total_stations = {}
	total_reviews = 0
	total_better_episodes = 0
	percentage_reviews = 0
	total_number_of_programs = 0

	import csv
	response = HttpResponse(content_type='text/csv')
	response['Content-Disposition'] = 'attachment; filename="Uliza-Log-export-'+request.GET['start_date']+'-'+request.GET['end_date']+'.csv"'
	writer = csv.writer(response)
	writer.writerow(['Radio series code','Public name','Radio station name','Project name','Country','Episodes aired','Episodes reviewed','Airtime (mins)','start date','end_date'])

	weekdays = ( 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')


	for program in programs:
		number_of_episodes = 0
		start_week_number=1
		better_scores = 0
		end_week_number = program.weeks
		program.start_date = program.start_date.replace(tzinfo=None)

		if program.start_date < start_date:
			week_diff = weeks_diff(program.start_date,start_date)

			start_week_number = week_diff[0]+1
			if program.start_date.weekday() <= start_date.weekday():
				start_week_number +=1


		if program.end_date > end_date:
			week_diff = weeks_diff(end_date,program.end_date)
			end_week_number = week_diff[0]
			end_week_number = program.weeks-end_week_number

			if end_date.weekday()>= program.start_date.weekday():
				end_week_number +=1

		number_of_episodes = end_week_number - start_week_number
		postponements = Log.objects.filter(program=program,postpone=True,week__gte=start_week_number,week__lte=end_week_number)
		number_of_episodes = number_of_episodes - len(postponements)

		if program.repeat_start_time:
			duration = program.duration*2
		else:
			duration = program.duration

		duration_multiplier = number_of_episodes

		if number_of_episodes > 0:
			# todo there are times where there could be duplicate reviews for an episode
			reviews = Review.objects.filter(log__week__gte=start_week_number,log__week__lte=end_week_number,log__program=program)
			if 'export' in request.GET:
				try:
					if not program.public_name:
						public_name = ''
					else:
						public_name = program.public_name
					writer.writerow([program.name.encode('utf8'),public_name.encode('utf8'),program.radio_station.name.encode('utf8'),program.project.name.encode('utf8'),program.project.country.name.encode('utf8'),int(number_of_episodes),len(reviews),program.duration,str(program.start_date),str(program.end_date)])
				except Exception as e:
					pass

			total_number_of_programs += 1
			start_week_number = int(start_week_number)
			start_week_number = int(start_week_number)
			better_scores = reviews.filter(numerical_score__gt=33)
			better_scores = len(better_scores)
			total_reviews += len(reviews)
			total_stations[program.radio_station.id] = program.radio_station.name
			total_episodes = total_episodes + number_of_episodes

			# total_hours will get into negative
			if program.repeat_start_time and program.repeat_week_day:
				if weekdays.index(program.repeat_week_day) <= program.start_date.weekday() or weekdays.index(program.repeat_week_day) > end_date.weekday():
					total_hours = total_hours - duration/2

			duration = duration*duration_multiplier
			total_hours = total_hours + duration


			if not program.poll_program_id:
				polls = PollSegment.objects.filter(program=program,episode_number__gte=start_week_number,episode_number__lte=end_week_number)
				total_polls += len(polls)
				responses = polls.aggregate(Sum('number_of_responses'))
				respondents = polls.aggregate(Sum('number_of_respondents'))
				if responses['number_of_responses__sum']:
					total_responses += responses['number_of_responses__sum']

				if respondents['number_of_respondents__sum']:
					average_respondents += respondents['number_of_respondents__sum']

		total_better_episodes += better_scores

	if total_episodes > 0:
		percentage_reviews = (total_reviews/total_episodes)*100

	if total_polls > 0:
		average_respondents = math.ceil(average_respondents/total_polls)

	total_hours = total_hours/60
	total_hours = math.floor(total_hours)
	percentage_reviews = math.floor(percentage_reviews)


	if 'export' in request.GET:
		return response

	return JsonResponse({'programs':total_number_of_programs,
						'number_of_better_episodes':total_better_episodes,
						'percentage_reviews':percentage_reviews,
						'number_of_reviews':total_reviews,
						'total_stations':len(total_stations.keys()),
						'total_episodes':int(total_episodes),
						'total_hours':total_hours,
						'total_responses':total_responses,
						'average_respondents':average_respondents},
						safe=False)

def interactivity(request):
	if 'program' in request.GET and not request.GET['program'] == 'all':
		program = Program.objects.get(id=request.GET['program'])

		# if a specific program is selected as a filter and if polls is linked to another program instance
		if program.poll_program_id:
			poll_segments = PollSegment.objects.filter(program__id=program.poll_program_id).order_by('episode_number')
			respondent_stat = RespondentStat.objects.filter(program__id=program.poll_program_id).order_by('episode_number')
		else:
			poll_segments = PollSegment.objects.filter(program__id=request.GET['program']).order_by('episode_number')
			respondent_stat = RespondentStat.objects.filter(program__id=request.GET['program']).order_by('episode_number')
	elif 'radio_station' in request.GET and not request.GET['radio_station'] == 'all':
		from django.db.models import Q
		programs = Program.objects.filter(radio_station=request.GET['radio_station'],project=request.GET['project']).exclude(poll_program_id=None).values_list('poll_program_id',flat=True)
		poll_segments = PollSegment.objects.filter(Q(program__id__in=programs)|Q(program__project=request.GET['project'],program__radio_station=request.GET['radio_station'])).order_by('episode_number')
		respondent_stat = RespondentStat.objects.filter(Q(program__id__in=programs)|Q(program__project=request.GET['project'],program__radio_station=request.GET['radio_station'])).order_by('episode_number')
	else:
		poll_segments = PollSegment.objects.filter(program__project=request.GET['project']).order_by('episode_number')
		respondent_stat = RespondentStat.objects.filter(program__project=request.GET['project']).order_by('episode_number')

	series = [[],[],[]]
	labels = []
	total_responses = 0
	unique_respondents = []
	episodes = 0
	programs = []
	week = None
	init_program = 0
	episode_respondents = []
	episode_responses = 0

	import json,itertools
	for poll in poll_segments:

		# checks if the polling data is shared with other programs
		if not poll.program.poll_program_id:
			if not poll.program.id in programs:
				episodes +=poll.program.weeks_aired()
				programs.append(poll.program.id)

			total_responses += poll.number_of_responses

			# the unique respondent number for each week is calculated here
			if week and not week == poll.episode_number:
				series[1].append(episode_responses)

				episode_respondent_stat = respondent_stat.filter(episode_number = week)
				if episode_respondent_stat:
					stat = episode_respondent_stat[0]
					episode_respondents = episode_respondents+json.loads(stat.unique_respondents_list)

				# filter out the redundunet occurences
				episode_respondents = set(itertools.chain(episode_respondents))

				# add episode respondents to all unique respondents list
				unique_respondents = unique_respondents + list(episode_respondents)

				episode_respondents = len(episode_respondents)
				series[0].append(episode_respondents+stat.repeat_respondents_number)


				# reset episode respondents list
				episode_respondents = []
				episode_responses = 0

				labels.append(label)

			label = 'Ep'+str(poll.episode_number)
			week = poll.episode_number
			episode_responses += poll.number_of_responses

			init_program = poll.program.id

	episode_respondent_stat = respondent_stat.filter(episode_number = week)
	if episode_respondent_stat:
		stat = episode_respondent_stat[0]
		episode_respondents = episode_respondents+json.loads(stat.unique_respondents_list)

	# Calculate the last iteration
	if episode_respondents:
		series[1].append(episode_responses)
		episode_respondents = set(itertools.chain(episode_respondents))
		unique_respondents = unique_respondents + list(episode_respondents)
		episode_respondents = len(episode_respondents)
		series[0].append(episode_respondents+stat.repeat_respondents_number)
		labels.append(label)

	unique_respondents = set(itertools.chain(unique_respondents))
	unique_respondents = len(unique_respondents)

	questions = len(poll_segments)
	response = {'series':series,'labels':labels,'total_responses':total_responses,'questions':questions,'unique_respondents':unique_respondents,'total_episodes':episodes}

	return JsonResponse(response,safe=False)

def interactivity_export(request):
	if 'program' in request.GET and not request.GET['program'] == 'all':
		program = Program.objects.get(id=request.GET['program'])

		# if a specific program is selected as a filter and if polls is linked to another program instance
		if program.poll_program_id:
			poll_segments = PollSegment.objects.filter(program__id=program.poll_program_id).order_by('program','episode_number')
		else:
			poll_segments = PollSegment.objects.filter(program__id=request.GET['program']).order_by('program','episode_number')
	elif 'radio_station' in request.GET and not request.GET['radio_station'] == 'all':
		from django.db.models import Q
		programs = Program.objects.filter(radio_station=request.GET['radio_station'],project=request.GET['project']).exclude(poll_program_id=None).values_list('poll_program_id',flat=True)
		poll_segments = PollSegment.objects.filter(Q(program__id__in=programs)|Q(program__project=request.GET['project'],program__radio_station=request.GET['radio_station'])).order_by('program','episode_number')
	else:
		poll_segments = PollSegment.objects.filter(program__project=request.GET['project']).order_by('program','episode_number')

	data = {}
	program_id_name = {}

	for poll in poll_segments:
		program_id_name[poll.program.id] = poll.program.name + 	"("+poll.program.start_date.strftime('%b %Y')+" - "+poll.program.end_date.strftime('%b %Y')+")"

		if not poll.program.id in data:
			data[poll.program.id] = {}

		if not poll.episode_number in data[poll.program.id]:
			data[poll.program.id][poll.episode_number] = {'interactions':poll.number_of_responses,'unique_respondents':poll.number_of_respondents}
		else:
			data[poll.program.id][poll.episode_number]['interactions'] += poll.number_of_responses
			if (poll.number_of_respondents>data[poll.program.id][poll.episode_number]['unique_respondents']):
				data[poll.program.id][poll.episode_number]['unique_respondents'] = poll.number_of_respondents

	data = [data,program_id_name]

	return JsonResponse(data,safe=False)

def export_respondents(request):
	# For testing export only episode 11
	if 'program' in request.GET and not request.GET['program'] == 'all':
		program = Program.objects.get(id=request.GET['program'])

		# if a specific program is selected as a filter and if polls is linked to another program instance
		if program.poll_program_id:
			respondent_stat = RespondentStat.objects.filter(program__id=program.poll_program_id).order_by('program','episode_number')
		else:
			respondent_stat = RespondentStat.objects.filter(program__id=request.GET['program']).order_by('program','episode_number')
	elif 'radio_station' in request.GET and not request.GET['radio_station'] == 'all':
		from django.db.models import Q
		programs = Program.objects.filter(radio_station=request.GET['radio_station'],project=request.GET['project']).exclude(poll_program_id=None).values_list('poll_program_id',flat=True)
		respondent_stat = RespondentStat.objects.filter(Q(program__id__in=programs)|Q(program__project=request.GET['project'],program__radio_station=request.GET['radio_station'])).order_by('program','episode_number')
	else:
		respondent_stat = RespondentStat.objects.filter(program__project=request.GET['project']).order_by('program','episode_number')

	respondent_list = {}
	program_id_name = {}
	import json
	for stat in respondent_stat:
		program_id_name[stat.program.id] = stat.program.name

		if not stat.program.id in respondent_list:
			respondent_list[stat.program.id] = []

		unique_respondents = json.loads(stat.unique_respondents_list)
		for new_respondents in unique_respondents:
			respondent_list[stat.program.id].append({'phone':new_respondents,'episode':stat.episode_number})
			# respondent_list[stat.program.id].append(new_respondents)

	# for x in respondent_list[stat.program.id]:
	# 	c = respondent_list[stat.program.id].count(x)
	# 	if c > 1:
	# 		print 'found',x
	# 	else:
	# 		print 'yes',c

	respondent_list = [respondent_list,program_id_name]

	return JsonResponse(respondent_list,safe=False)
