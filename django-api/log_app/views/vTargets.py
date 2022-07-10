from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
import django_filters
from rest_framework import filters

from log_app.models import Project,Program,Log,PollSegment,Review,Target,Contact,Report
from log_app.serializers import TargetSerializer
from django.http import HttpResponse
import datetime,math

class TargetGet(generics.ListCreateAPIView):
    queryset = Target.objects.all()
    model = Target
    serializer_class = TargetSerializer
    filter_fields = ['id','project']

class TargetEntity(generics.RetrieveUpdateAPIView):
    queryset = Target.objects.all()
    model = Target
    serializer_class = TargetSerializer
    lookup_field = 'id'

def set_targets(request,project_id):
    data = request.POST
    results = Target.objects.filter(project__id=project_id,custom=False)
    previous_results = results.values_list('variable_identifier',flat=True)

    project = Project.objects.get(id = project_id)


    # for each target check if previously saved & update
    for target in data.keys():
        if target in previous_results:
            result = Target.objects.get(project__id=project_id,variable_identifier=target)
            result.target_value = data[target]
            result.save()
        else:
            if not data[target] == '':
                Target.objects.create(project=project,variable_identifier=target,target_value=data[target])

    return HttpResponse('Processed')

def weeks_diff(start,end):
 monday = (start - datetime.timedelta(days=start.weekday()))
 calc = (end - monday).days
 weeks = math.floor(calc/7.0)
 remaining_days = calc%7
 return weeks,remaining_days

def target_stats(request):
    from django.utils.dateparse import parse_date,parse_datetime
    from django.db.models import Sum,Avg
    start_date = parse_datetime(request.GET['start_date']+' 00:00')
    end_date = parse_date(request.GET['end_date'])

    project = Project.objects.get(id=request.GET['project_id'])
    programs = Program.objects.filter(project=project,end_date__gte = start_date,start_date__lte=end_date).exclude(project__country__exclude=True)

    total_episodes = 0
    total_hours = 0
    total_responses = 0
    average_respondents = 0
    total_polls = 0
    total_stations = {}
    total_languages = {}
    total_reviews = 0
    total_better_episodes = 0
    percentage_reviews = 0
    total_number_of_programs = 0
    impact_stations = {}
    network_stations = {}

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

            if program.broadcast_language:
                total_languages[program.broadcast_language.id] = program.broadcast_language.name

            total_episodes = total_episodes + number_of_episodes

            if program.implementation_type == 'impact':
                impact_stations[program.radio_station.id] = program.radio_station.name
            elif program.implementation_type == 'network':
                network_stations[program.radio_station.id] = program.radio_station.name

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
        percentage_reviews = (float(total_reviews)/total_episodes)*100

    if total_polls > 0:
        average_respondents = math.ceil(average_respondents/total_polls)

    total_hours = total_hours/60
    impact_stations = len(impact_stations)
    total_languages = len(total_languages)
    network_stations = len(network_stations)
    total_stations = len(total_stations)
    if total_stations > 0:
        episode_length_avg = total_hours/total_stations
    else:
        episode_length_avg = 0

    episode_length_avg = math.floor(episode_length_avg)
    total_hours = math.floor(total_hours)
    percentage_reviews = math.floor(percentage_reviews)

    project_results = Target.objects.filter(project__id=project.id)
    targets = {}

    from django.contrib.humanize.templatetags.humanize import naturalday
    for r in project_results:
        if r.last_updated_by:
            last_updated_by = r.last_updated_by.first_name+' '+r.last_updated_by.last_name
        else:
            last_updated_by = None

        targets[r.variable_identifier] = {'id':r.id,'target':r.target_value,'value':r.value,'last_updated_at':naturalday(r.last_updated_at),'last_updated_by':last_updated_by}

    if 'export' in request.GET:
        return response

    return JsonResponse({'programs':total_number_of_programs,
						'number_of_better_episodes':total_better_episodes,
						'percentage_reviews':percentage_reviews,
						'number_of_reviews':total_reviews,
						'total_stations': total_stations,
						'total_episodes':int(total_episodes),
						'total_polls':total_polls,
						'total_languages':total_languages,
						'total_hours':total_hours,
						'total_responses':total_responses,
						'impact_stations':impact_stations,
						'network_stations':network_stations,
						'episode_length_avg':episode_length_avg,
						'average_respondents':average_respondents,
                        'targets': targets},
						safe=False)
