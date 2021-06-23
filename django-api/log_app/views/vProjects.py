from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Project,Review,Checklist,Format,Comment,Log,BroadcasterResource
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
	ordering_fields=('id','country','created_at')
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

	if not 'lang' in request.GET or request.GET['lang'] == 'en':
		lang = ''
	else:
		lang = '_'+request.GET['lang']

	if 'start_date' in request.GET and 'end_date' in request.GET:
		from django.utils.dateparse import parse_date
		start_date = parse_date(request.GET['start_date'])
		end_date = parse_date(request.GET['end_date'])
		comments = Comment.objects.filter(log__program__project=project_id,\
		                                      created_at__gte=start_date,\
		                                      created_at__lte=end_date)

		logs = Log.objects.filter(program__project = project_id,\
								  created_at__gte=start_date,\
		                          created_at__lte=end_date, postpone=False).order_by('week')

	else:
		comments = Comment.objects.filter(log__program__project=project_id)
		logs = Log.objects.filter(program__project = project_id,postpone=False).order_by('week')


	if 'program' in request.GET:
		if not request.GET['program'] == 'all':
			comments = comments.filter(log__program=request.GET['program'])
			logs = logs.filter(program = request.GET['program'])
	elif 'radio_station' in request.GET:
		if not request.GET['radio_station'] == 'all':
			comments = comments.filter(log__program__radio_station=request.GET['radio_station'])
			logs = logs.filter(program__radio_station = request.GET['radio_station'])



	kp_comments = len(comments.filter(contact__role__in=['knowledge_partner','gender_specialist']))
	gender_comments = len(comments.filter(contact__role='gender_specialist'))
	training_calls = len(comments.filter(training_call=True))

	comments = len(comments)

	comments = {'comments':comments,\
				'training_calls': training_calls,\
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
	formats_project_related = Format.objects.filter(projects=project_id)
	formats_always_checked = Format.objects.filter(always_checked=True)
	formats_always_checked = formats_always_checked | formats_project_related
	checklists = Checklist.objects.all()

	from itertools import chain
	import math

	format_score = []
	format_index = {}
	labels = []

	level_score = {'good':1,'better':2,'best':3}

	week=None
	voice_score = 0
	gender_score=None
	logs_reviewed=0

	week_labels = []
	week_scores = []

	cross_formats = Format.objects.filter(cross_checklist=True)
	secondary_criteria = {}

	for cformat in cross_formats:
		for id in cformat.secondary_checklist.all():
			if not id in secondary_criteria:
				secondary_criteria[id] = [cformat.id]
			else:
				secondary_criteria[id].append(cformat.id)

	cf_scores =  {}
	for log in logs:
	    if week == (log.week,log.program.id):
	        continue

	    week = (log.week,log.program.id)
	    review = Review.objects.filter(log=log)

	    if not review:
	        continue
	    else:
			logs_reviewed = logs_reviewed+1
			review=review[0]

	    rtype_formats = log.program.radio_type.values_list('id',flat=True)
	    if rtype_formats:
			rtype_formats = Format.objects.filter(radio_type_related=True,radio_types__in=rtype_formats)
	    else:
			rtype_formats = []

	    log_formats = log.formats.filter(legacy=False)
	    log_formats = list(chain(log_formats,formats_always_checked,rtype_formats))

	    review_checklists = review.checklists.values_list('id',flat=True)
	    void_formats = review.void_formats.values_list('id',flat=True)

	    for format in log_formats:
			score = 0
			total_score = 0

			format_checklists = checklists.filter(radio_format=format,created_at__lte=review.last_updated_at)

			for criteria in format_checklists:
				if criteria.id in review_checklists:
				    score = level_score[criteria.level]+score

				else:
					if format.id in void_formats:
					    score = -level_score[criteria.level]+score

				total_score = level_score[criteria.level]+total_score

				if 'format_id' in request.GET and str(format.id) == request.GET['format_id']:
					week_label = str(log.week)
					if not week_label in week_labels:
						week_labels.append(week_label)
						week_scores.append({'meta': week_label,'value':0,'total':0})

					index = week_labels.index(week_label)

					if criteria.id in review_checklists:
						week_scores[index]['value'] = week_scores[index]['value']+level_score[criteria.level]
					else:
						if format.id in void_formats:
							week_scores[index]['value'] = week_scores[index]['value']-level_score[criteria.level]

					week_scores[index]['total'] = week_scores[index]['total']+level_score[criteria.level]


				if criteria.id in secondary_criteria:
					for cross_format in secondary_criteria[criteria.id]:

						if not cross_format.id in format_index.keys():
							format_index[cross_format.id] = len(format_score)

							if getattr(cross_format,'name'+lang):
							    labels.append(getattr(cross_format,'name'+lang))
							    format_score.append({'meta':getattr(cross_format,'name'+lang),'value':0,'total_score':0,'use':0})
							else:
							    labels.append(getattr(cross_format,'name'))
							    format_score.append({'meta':getattr(cross_format,'name'),'value':0,'total_score':0,'use':0})

						if criteria.id in review_checklists:
							format_score[format_index][cross_format.id]['value'] = level_score[criteria.level]+format_score[format_index][cross_format.id]['value']
						else:
							if cross_format.id in void_formats:
								format_score[format_index][cross_format.id]['value'] = -level_score[criteria.level]+format_score[format_index][cross_format.id]['value']

						format_score[format_index][cross_format.id]['total_score'] = format_score[format_index][cross_format.id]['total_score'] + level_score[criteria.level]

						if 'format_id' in request.GET and str(cross_format.id) == request.GET['format_id']:
							week_label = str(log.week)
							if not week_label in week_labels:
								week_labels.append(week_label)
								week_scores.append({'meta': week_label,'value':0,'total':0})

							index = week_labels.index(week_label)

							if criteria.id in review_checklists:
								week_scores[index]['value'] = week_scores[index]['value']+level_score[criteria.level]
							else:
								if cross_format.id in void_formats:
									week_scores[index]['value'] = week_scores[index]['value']-level_score[criteria.level]

							week_scores[index]['total'] = week_scores[index]['total']+level_score[criteria.level]

			if not format.id in format_index.keys():
				format_index[format.id] = len(format_score)

				if getattr(format,'name'+lang):
				    labels.append(getattr(format,'name'+lang))
				    format_score.append({'meta':getattr(format,'name'+lang),'value':0,'total_score':0,'use':0})
				else:
				    labels.append(getattr(format,'name'))
				    format_score.append({'meta':getattr(format,'name'),'value':0,'total_score':0,'use':0})


			format_score[format_index[format.id]]['value'] = format_score[format_index[format.id]]['value']+score
			format_score[format_index[format.id]]['total_score'] = total_score+format_score[format_index[format.id]]['total_score']
			format_score[format_index[format.id]]['use'] = format_score[format_index[format.id]]['use']+1


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
		# initializing
		# if not format.id in format_index:
		if not format.id in format_index or format_score[format_index[format.id]]['total_score'] == 0:
			pass

			# format_index[format.id] = len(format_score)
			#
			# if getattr(format,'name'+lang):
			#     labels.append(getattr(format,'name'+lang))
			#     format_score.append({'meta':getattr(format,'name'+lang),'value':0,'logs':0})
			# else:
			#     labels.append(getattr(format,'name'))
			#     format_score.append({'meta':getattr(format,'name'),'value':0,'logs':0})
			#
			# if least_used['value'] == 0:
			#     least_used['formats_index'].append(format_index[format.id])
			# else:
			#     least_used['formats_index'] = [format_index[format.id]]
			#     least_used['value'] = 0
		else:
			format_score[format_index[format.id]]['value'] = (float(format_score[format_index[format.id]]['value'])/format_score[format_index[format.id]]['total_score'])*100
			format_score[format_index[format.id]]['value'] = math.ceil(format_score[format_index[format.id]]['value'])

			if format.name == 'Gender':
				gender_score = format_score[format_index[format.id]]['value']

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
			if not format.always_checked and not format.project_related:
			    # select most used format
			    if format_score[format_index[format.id]]['use'] > most_used['value']:
			        most_used['formats_index'] = [format_index[format.id]]
			        most_used['value'] = format_score[format_index[format.id]]['use']
			    elif format_score[format_index[format.id]]['use'] == most_used['value']:
			        most_used['formats_index'].append(format_index[format.id])

			    # select least used format
			    if format_score[format_index[format.id]]['use'] < least_used['value']:
			        least_used['formats_index'] = [format_index[format.id]]
			        least_used['value'] = format_score[format_index[format.id]]['use']
			    elif format_score[format_index[format.id]]['use'] == least_used['value']:
			        least_used['formats_index'].append(format_index[format.id])

		# base weak value
		if weak['formats_index'] == [] and format_score:
		    weak['value'] = format_score[format_index[format.id]]['value']
		    weak['formats_index'].append(format_index[format.id])

		# base least used value
		if least_used['formats_index'] == [] and format_score:
		    least_used['value'] = format_score[format_index[format.id]]['total_score']
		    least_used['formats_index'].append(format_index[format.id])


	if 'format_id' in request.GET:
		for week_score in week_scores:
			week_score['value'] = float(week_score['value'])/week_score['total']*100
			week_score['value'] = math.ceil(week_score['value'])

		format_score = week_scores

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

	# prepare broadcaster resources data
	# [{'broadcaster_resource__name': u'name', 'total': 2}]
	from django.db.models import Count
	resources = logs.exclude(broadcaster_resource=None).order_by('broadcaster_resource').values('broadcaster_resource__name').annotate(total=Count('broadcaster_resource'))
	resources_null = list(BroadcasterResource.objects.values_list('name',flat=True))

	br = {'most_used':["<None>"],'least_used':["<None>"],'total_use':0}
	if resources:
		br['least_used'] = []
		br_mu = 0
		br_lu =resources[0]
		for resource in resources:
			resources_null.remove(resource['broadcaster_resource__name'])
			br['total_use'] = br['total_use']+resource['total']
			if resource['total']> br_mu :
				br_mu = resource['total']
				br['most_used'] = [resource['broadcaster_resource__name']]
			elif resource['total']==br_mu and not br_mu == 0 and not resource['broadcaster_resource__name'] in br['most_used']:
				br['most_used'].append(resource['broadcaster_resource__name'])

			if resource['total']<br_lu:
				br_lu = resource['total']
				br['least_used'] = [resource['broadcaster_resource__name']]
			elif resource['total'] == br_lu and not resource['broadcaster_resource__name'] in br['least_used']:
				br['least_used'].append(resource['broadcaster_resource__name'])

		if resources_null:
			br['least_used'] = resources_null

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

	if week_labels:
		format_score['week_labels'] = week_labels

	return JsonResponse({'format_score':format_score,\
						 'br':br,\
	                     'comments':comments},safe=False)
