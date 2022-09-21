from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
import django_filters
from rest_framework import filters

from log_app.models import Project,Program,Log,PollSegment,Review,Target,Contact,Report,Indicator
from log_app.serializers import TargetSerializer
from django.http import HttpResponse
import datetime,math

class TargetGet(generics.ListCreateAPIView):
    queryset = Target.objects.all()
    model = Target
    serializer_class = TargetSerializer
    filter_fields = ['id','project','indicator','project__code']

class TargetEntity(generics.RetrieveUpdateAPIView):
    queryset = Target.objects.all()
    model = Target
    serializer_class = TargetSerializer
    lookup_field = 'id'

def set_targets(request,project_id):
    data = request.POST
    targets = Target.objects.filter(project__id=project_id)
    previous_targets = targets.values_list('indicator__id',flat=True)
    previous_targets = list(previous_targets)

    project = Project.objects.get(id = project_id)


    # for each target check if previously saved & update
    for target in data.keys():
        if int(target) in previous_targets:
            indicator = Indicator.objects.get(id=target)
            t = Target.objects.filter(project__id=project_id,indicator=indicator).last()

            if not data[target] == '':
                previous_targets.pop(previous_targets.index(int(target)))
                t.target_value = data[target]
                t.save()
        else:
            if not data[target] == '':
                indicator = Indicator.objects.get(id=target)
                response = Target.objects.create(project=project,indicator=indicator,target_value=data[target])

    if previous_targets:
        targets = Target.objects.filter(indicator__id__in=previous_targets,project__id = project_id)
        targets.delete()

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

    project = Project.objects.filter(id=request.GET['project_id'])
    if request.GET['multi_country'] == 'true':
        project = Project.objects.filter(code=project[0].code)

    if 'start_date' in request.GET and 'end_date' in request.GET:
        start_datetime = parse_datetime(request.GET['start_date']+' 00:00')
        start_date = parse_date(request.GET['start_date'])
        end_date = parse_date(request.GET['end_date'])
        programs = Program.objects.filter(project__in=project,end_date__gte = start_datetime,start_date__lte=end_date).exclude(project__country__exclude=True)
    else:
        # I was born 08-04-1991
        start_datetime = datetime.datetime(year=1991,month=4,day=8)
        start_date = datetime.date(year=1991,month=4,day=8)
        end_date = datetime.date(year=project[0].end_date.year+5,month=project[0].end_date.month,day=1)
        programs = Program.objects.filter(project__in=project).exclude(project__country__exclude=True)

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
    episode_length_avg = 0
    impact_stations = {}
    network_stations = {}

    weekdays = ( 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')


    for program in programs:
        number_of_episodes = 0
        episode_length_avg += program.duration
        start_week_number=1
        better_scores = 0
        end_week_number = program.weeks
        program.start_date = program.start_date.replace(tzinfo=None)

        if program.start_date < start_datetime:
            week_diff = weeks_diff(program.start_date,start_datetime)

            start_week_number = week_diff[0]+1
            if program.start_date.weekday() <= start_datetime.weekday():
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
            better_scores = reviews.filter(numerical_score__gt=50)
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

    if total_number_of_programs > 0:
        episode_length_avg = episode_length_avg/total_number_of_programs

    total_hours = math.floor(total_hours)
    percentage_reviews = math.floor(percentage_reviews)

    indicators = Indicator.objects.all().values()
    # targets = Target.objects.filter(project__id=project.id)

    results = []

    from django.contrib.humanize.templatetags.humanize import naturalday
    for i in indicators:
        t = Target.objects.filter(indicator__id=i['id'],project__in=project).order_by('last_updated_at')
        i['editing'] = False
        i['saving'] = False
        i['created_at'] = ''

        if t:
            # t = t[0]
            if t.last().last_updated_by:
                last_updated_by = t.last().last_updated_by.first_name+' '+t.last_updated_by.last_name
            else:
                last_updated_by = None

            result  = {}
            i['target'] = t.aggregate(Sum('target_value'))['target_value__sum']
            i['total_result'] = t.aggregate(Sum('value'))['value__sum']

            result = Report.objects.filter(target__in=t,report_date__gte=start_date,report_date__lte=end_date).aggregate(Sum('value'))
            if not result['value__sum']:
                i['requested_result'] = 0
                i['result'] = 0
            else:
                i['requested_result'] = result['value__sum']
                i['result'] = result['value__sum']

            i['last_updated_at'] = naturalday(t.last().last_updated_at)
            i['last_updated_by'] = last_updated_by
        else:
            i['target'] = -1
            i['result'] = 0
            i['target_id'] = None
            i['last_updated_at'] = None


        results.append(i)


    import json
    return JsonResponse({'programs':total_number_of_programs,
						'number_of_better_episodes':total_better_episodes,
						'percentage_reviews':percentage_reviews,
						'number_of_reviews':total_reviews,
						'total_stations': total_stations,
						'unknown': 'TBD',
						'total_episodes':int(total_episodes),
						'total_polls':total_polls,
						'total_languages':total_languages,
						'total_hours':total_hours,
						'total_responses':total_responses,
						'impact_stations':impact_stations,
						'network_stations':network_stations,
						'episode_length_avg':episode_length_avg,
						'average_respondents':average_respondents,
                        'results': results},safe=False)
