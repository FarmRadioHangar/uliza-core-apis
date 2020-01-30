from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Project,Review,Checklist,Format,Comment,Log
from log_app.serializers import ProjectSerializer
from rest_framework.pagination import PageNumberPagination
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework.decorators import api_view

import django_filters
from rest_framework import filters

voice_format_code = 'VOICE';

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
	filter_backends = (filters.OrderingFilter,filters.SearchFilter,DjangoFilterBackend)
	pagination_class = LargeResultsSetPagination
	fields=['id']
	search_fields = ('name','doner','country__name')

class ProjectEntity(generics.RetrieveUpdateAPIView):
	queryset = Project.objects.all()
	model = Project
	serializer_class = ProjectSerializer
	lookup_field = 'id'



@api_view(['GET'])
def project_report_numbers(request,project_id):
	"""
	Return the stats from specific project related to comments posted
	100 # comments | 70 # training calls
	100 # comments by Knowledge partners
	100 # comments by Gender specialist
	"""

	if 'start_date' in request.GET and 'end_date' in request.GET:
		from django.utils.dateparse import parse_date
		start_date = parse_date(request.GET['start_date'])
		end_date = parse_date(request.GET['end_date'])
		comments = Comment.objects.filter(log__program__project=project_id,\
		                                      created_at__gte=start_date,\
		                                      created_at__lte=end_date)

		logs = Log.objects.filter(program__project = project_id,\
								  created_at__gte=start_date,\
		                          created_at__lte=end_date)

	else:
		comments = Comment.objects.filter(log__program__project=project_id)
		logs = Log.objects.filter(program__project = project_id)


	if 'radio_station' in request.GET:
		if not request.GET['radio_station'] == 'all':
			comments = comments.filter(log__program__radio_station=request.GET['radio_station'])
			logs = logs.filter(program__radio_station = request.GET['radio_station'])



	kp_comments = len(comments.filter(contact__role__in=['knowledge_partner','project_partner']))
	gender_comments = len(comments.filter(contact__role='consultant'))

	comments = len(comments)

	comments = {'comments':comments,\
	            'kp_comments':kp_comments,\
	            'gender_comments':gender_comments}
	"""
	Return the stats from specific project related to GBB reviews
	76% Gender score
	89% VOICE score
	100% episodes analysed
	Vox pop Weakest | Vox pop Strongest
	60% Vox pop | 50% Interview
	Vox pop Least used | Vox pop Most used
	"""
	# radio station filter comes here
	formats_always_checked = Format.objects.filter(always_checked=True)
	checklists = Checklist.objects.all()

	from itertools import chain
	import math

	format_score = []
	format_index = {}
	labels = []

	level_score = {'good':1,'better':2,'best':3}

	week=0
	voice_score = 0
	gender_score=0
	total_gender_score = 0
	logs_reviewed=0
	for log in logs:
	    if week == log.week:
	        continue

	    week = log.week
	    review = Review.objects.filter(log=log)

	    if not review:
	        continue
	    else:
			logs_reviewed = logs_reviewed+1
			review=review[0]

	    log_formats = log.formats.all()
	    log_formats = list(chain(log_formats,formats_always_checked))

	    review_checklists = review.checklists.values_list('id',flat=True)
	    for format in log_formats:
	        score = 0
	        total_score = 0

			# review created_at should be considered here
	        format_checklists = checklists.filter(radio_format=format,created_at__lte=review.last_updated_at)

	        for criteria in format_checklists:
	            if criteria.id in review_checklists:
	                score = level_score[criteria.level]+score

	                if criteria.gender_responsive:
						gender_score = level_score[criteria.level]+gender_score

	            total_gender_score = level_score[criteria.level]+total_gender_score
	            total_score = level_score[criteria.level]+score

	        if not format.id in format_index.keys():
	            format_index[format.id] = len(format_score)
	            labels.append(format.name)
	            format_score.append({'meta':format.name,'value':0,'logs':0})

	        if total_score>0:
				score = (float(score)/total_score)*100
				score = math.ceil(score)

	        else:
	            score = 0

	        format_score[format_index[format.id]]['value'] = format_score[format_index[format.id]]['value']+score
	        format_score[format_index[format.id]]['logs'] = format_score[format_index[format.id]]['logs']+1

	formats = Format.objects.filter(legacy=False)

	# Weakest | Strongest
	# Least used | Most used
	weak = {'value':0,'formats_index':[]}
	strong = {'value':0,'formats_index':[]}
	most_used = {'value':0,'formats_index':[]}
	least_used = {'value':0,'formats_index':[]}

	if logs:
		logs_reviewed = float(logs_reviewed)/len(logs)*100
		logs_reviewed = math.ceil(logs_reviewed)
	else:
		logs_reviewed = 0

	reviewed_formats = len(format_index)
	total_score = 0

	for format in formats:

		if not format.id in format_index:
		    format_index[format.id] = len(format_score)
		    format_score.append({'meta':format.name,'value':0,'logs':0})
		    labels.append(format.name)

		    if least_used['value'] == 0:
		        least_used['formats_index'].append(format_index[format.id])
		    else:
		        least_used['formats_index'] = [format_index[format.id]]
		        least_used['value'] = 0
		else:
			format_score[format_index[format.id]]['value'] = (float(format_score[format_index[format.id]]['value'])/format_score[format_index[format.id]]['logs'])
			format_score[format_index[format.id]]['value'] = math.ceil(format_score[format_index[format.id]]['value'])
			total_score = total_score +format_score[format_index[format.id]]['value']

			# VOICE
			if format.name == voice_format_code:
				voice_score = format_score[format_index[format.id]]['value']

			if format_score[format_index[format.id]]['value'] > strong['value']:
			    strong['formats_index'] = [format_index[format.id]]
			    strong['value'] = format_score[format_index[format.id]]['value']
			elif format_score[format_index[format.id]]['value'] == strong['value']:
			    strong['formats_index'].append(format_index[format.id])

			# select weak format
			if format_score[format_index[format.id]]['value'] < weak['value']:
			    weak['formats_index'] = [format_index[format.id]]
			    weak['value'] = format_score[format_index[format.id]]['value']
			elif format_score[format_index[format.id]]['value'] == weak['value']:
			    weak['formats_index'].append(format_index[format.id])

			# if not format.always_checked [if always_checked is not included in this calculation]
			if not format.always_checked:
			    # select most used format
			    if format_score[format_index[format.id]]['logs'] > most_used['value']:
			        most_used['formats_index'] = [format_index[format.id]]
			        most_used['value'] = format_score[format_index[format.id]]['logs']
			    elif format_score[format_index[format.id]]['logs'] == most_used['value']:
			        most_used['formats_index'].append(format_index[format.id])

			    # select least used format
			    if format_score[format_index[format.id]]['logs'] < least_used['value']:
			        least_used['formats_index'] = [format_index[format.id]]
			        least_used['value'] = format_score[format_index[format.id]]['logs']
			    elif format_score[format_index[format.id]]['logs'] == least_used['value']:
			        least_used['formats_index'].append(format_index[format.id])

		# base weak value
		if weak['formats_index'] == []:
		    weak['value'] = format_score[format_index[format.id]]['value']
		    weak['formats_index'].append(format_index[format.id])

		# base least used value
		if least_used['formats_index'] == []:
		    least_used['value'] = format_score[format_index[format.id]]['logs']
		    least_used['formats_index'].append(format_index[format.id])


	if total_gender_score > 0:
		gender_score = float(gender_score)/total_gender_score
		gender_score = math.ceil(gender_score*100)


	if reviewed_formats > 0:
		total_score = total_score/reviewed_formats
		if total_score <=33 and total_score >0:
			total_score = 'good'
		elif 33<total_score and total_score <= 66:
			total_score = 'better'
		elif total_score > 66:
			total_score = 'best'
	else:
		total_score = '-'

	format_score.append({'meta':'Gender','value':gender_score})
	labels.append('Gender')
	format_score = {'series':[format_score],\
					'most_used':most_used,\
					'least_used':least_used,\
					'weak':weak,\
					'strong':strong,\
					'gender_score':gender_score,\
					'voice_score':voice_score,\
					'logs_reviewed':logs_reviewed,\
					'total_score':total_score,\
					'labels':labels}



	return JsonResponse({'format_score':format_score,\
	                     'comments':comments},safe=False)
