from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import *
from log_app.serializers import ReviewSerializer
import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework import filters

class ReviewGet(generics.ListCreateAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    ordering_fields = ('id','created_at')
    filter_fields = ['id', 'log', 'reviewer']

    def get_queryset(self):
        program = self.request.GET.get('program')
        projects = self.request.GET.get('projects')
        if program:
            logs = Log.objects.filter(program=program).values('id')
            queryset = Review.objects.filter(log__in=logs)
        elif projects:
            pk_list = projects.split(',')
            logs = Log.objects.filter(program__project__in=pk_list).values('id')
            queryset = Review.objects.filter(log__in=logs)
        else:
            queryset = Review.objects.all()

        return queryset


class ReviewEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    lookup_field = 'id'


def calculate_result(log):
	"""
	Return the stats from specific project related to GBB reviews
	76% Gender score
	89% VOICE score
	100% episodes analysed
	Vox pop Weakest | Vox pop Strongest
	60% Vox pop | 50% Interview
	Vox pop Least used | Vox pop Most used
	"""
	lang = ''
	voice_format_code = 'VOICE';
	# logs = Log.objects.filter(program__project = project_id,postpone=False).order_by('week')
	project_id = log.program.project.id
	logs = [log]

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
		for criteria in cformat.secondary_checklist.all():
			if not criteria in secondary_criteria:
				secondary_criteria[criteria.id] = [cformat]
			else:
				secondary_criteria[criteria.id].append(cformat)

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

				# if 'format_id' in request.GET and str(format.id) == request.GET['format_id']:
				# 	week_label = str(log.week)
				# 	if not week_label in week_labels:
				# 		week_labels.append(week_label)
				# 		week_scores.append({'meta': week_label,'value':0,'total':0})
                #
				# 	index = week_labels.index(week_label)
                #
				# 	if criteria.id in review_checklists:
				# 		week_scores[index]['value'] = week_scores[index]['value']+level_score[criteria.level]
				# 	else:
				# 		if format.id in void_formats:
				# 			week_scores[index]['value'] = week_scores[index]['value']-level_score[criteria.level]
                #
				# 	week_scores[index]['total'] = week_scores[index]['total']+level_score[criteria.level]
                #

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
							format_score[format_index[cross_format.id]]['value'] = level_score[criteria.level]+format_score[format_index[cross_format.id]]['value']
						else:
							if cross_format.id in void_formats:
								format_score[format_index[cross_format.id]]['value'] = -level_score[criteria.level]+format_score[format_index[cross_format.id]]['value']

						format_score[format_index[cross_format.id]]['total_score'] = format_score[format_index[cross_format.id]]['total_score'] + level_score[criteria.level]

						# if 'format_id' in request.GET and str(cross_format.id) == request.GET['format_id']:
						# 	week_label = str(log.week)
						# 	if not week_label in week_labels:
						# 		week_labels.append(week_label)
						# 		week_scores.append({'meta': week_label,'value':0,'total':0})
                        #
						# 	index = week_labels.index(week_label)
                        #
						# 	if criteria.id in review_checklists:
						# 		week_scores[index]['value'] = week_scores[index]['value']+level_score[criteria.level]
						# 	else:
						# 		if cross_format.id in void_formats:
						# 			week_scores[index]['value'] = week_scores[index]['value']-level_score[criteria.level]
                        #
						# 	week_scores[index]['total'] = week_scores[index]['total']+level_score[criteria.level]

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

	total_score = 0

	for format in formats:
		# initializing
		# if not format.id in format_index:
		if not format.id in format_index or format_score[format_index[format.id]]['total_score'] == 0:
			pass
		else:
			format_score[format_index[format.id]]['value'] = (float(format_score[format_index[format.id]]['value'])/format_score[format_index[format.id]]['total_score'])*100
			format_score[format_index[format.id]]['value'] = math.ceil(format_score[format_index[format.id]]['value'])

			if format.name == 'Gender':
				gender_score = format_score[format_index[format.id]]['value']

			total_score = total_score +format_score[format_index[format.id]]['value']

			# VOICE
			if format.name == voice_format_code:
				voice_score = format_score[format_index[format.id]]['value']


	return format_score



def export_analysis(request):
    project = request.GET['project']

    from log_app.models import Program, Review
    project = Project.objects.get(id=project)
    programs = Program.objects.filter(project=project)

    from django.http import HttpResponse
    import csv
    response = HttpResponse(content_type='text/csv')
    filename=project.name.encode('utf8')+'_'+project.country.name.encode('utf8')
    response['Content-Disposition'] = 'attachment; filename="'+filename+'"'
    writer  = csv.writer(response)
    current_program = None

    data = []
    for program in programs:
        result = {}
        result['radio_station_name'] = program.radio_station
        result['country_name'] = program.radio_station.country.name

        reviews = Review.objects.filter(log__program=program).order_by('log__week')

        result['result'] = []
        import math
        for review in reviews:
            result = calculate_result(review.log)
            sorted_result = {}

            technical_score = 0
            technical_number = 0
            overall_score = 0
            overall_number = 0
            for r in result:
                sorted_result[r['meta']] = r['value']

                if not r['meta'] == 'Interactivity' and not r['meta'] == 'Gender':
                    technical_score += r['value']
                    technical_number +=1

                # voice is already included in the technical score
                if r['meta'] == 'Interactivity' or r['meta'] == 'Gender':
                    overall_score += r['value']

                if not 'numerical' in request.GET:
                    if r['value'] <= 0:
                        sorted_result[r['meta']] = 'Null'
                    elif r['value'] <= 33:
                        sorted_result[r['meta']] = 'Good'
                    elif r['value'] > 33 and r['value'] <= 66:
                        sorted_result[r['meta']] = 'Better'
                    else:
                        sorted_result[r['meta']] = 'Best'
                else:
                    sorted_result[r['meta']] = r['value']

            technical_score = (float(technical_score)/technical_number)
            technical_score = math.ceil(technical_score)
            overall_score += technical_score

            # the overall_number is technical_number plus 2
            overall_number = technical_number+2
            overall_score = (float(overall_score)/overall_number)
            overall_score = math.ceil(overall_score)

            if not 'numerical' in request.GET:
                if technical_score <= 0:
                    sorted_result['Technical'] = 'Null'
                elif technical_score <= 33:
                    sorted_result['Technical'] = 'Good'
                elif technical_score > 33 and technical_score <= 66:
                    sorted_result['Technical'] = 'Better'
                else:
                    sorted_result['Technical'] = 'Best'
            else:
                sorted_result['Technical'] = technical_score

            if not 'numerical' in request.GET:
                if overall_score <= 0:
                    sorted_result['Overall'] = 'Null'
                elif overall_score <= 33:
                    sorted_result['Overall'] = 'Good'
                elif overall_score > 33 and overall_score <= 66:
                    sorted_result['Overall'] = 'Better'
                else:
                    sorted_result['Overall'] = 'Best'
            else:
                sorted_result['Overall'] = overall_score

            data.append({'radio_station':review.log.program.radio_station.name.encode('utf8'),'program': review.log.program.name.encode('utf8'),'program_id':review.log.program.id,'week':review.log.week,'country':review.log.program.radio_station.country.name.encode('utf8'),'result':sorted_result})


    row_data = []
    for d in data:
        if not d['program_id'] == current_program:
            if row_data:
                writer.writerow(row_data)

            current_program = d['program_id']
            row_data = [d['radio_station'],d['country'],d['program'],d['week'],d['result']['Overall'],d['result']['Technical'],d['result']['VOICE'],d['result']['Interactivity'],d['result']['Gender']]
        else:
            row_data = row_data + [d['week'],d['result']['Overall'],d['result']['Technical'],d['result']['VOICE'],d['result']['Interactivity'],d['result']['Gender']]


    if row_data:
        writer.writerow(row_data)

    return response
