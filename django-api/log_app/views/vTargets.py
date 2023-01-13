from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework import filters

from log_app.models import Project,Program,Log,PollSegment,Review,Target,Contact,Report,Indicator,RespondentStat
from log_app.serializers import TargetSerializer
from django.http import HttpResponse
import datetime,math

class TargetGet(generics.ListCreateAPIView):
    queryset = Target.objects.all()
    model = Target
    serializer_class = TargetSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    filter_fields = ['id','project','indicator','project__code','project__country']

    def get_queryset(self):
        project_list = self.request.GET.get('project__id__in')

        if project_list:
            project_list = project_list.split(',')
            project_list = Project.objects.filter(id__in=project_list)
            queryset = Target.objects.filter(project__in=project_list)
        else:
            queryset = Target.objects.all()

        return queryset

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
    import json
    for target in data.keys():
        payload = json.loads(data[target])

        # update target
        if int(target) in previous_targets:
            indicator = Indicator.objects.get(id=target)
            t = Target.objects.filter(project__id=project_id,indicator=indicator).last()

            if not payload['value'] == '':
                previous_targets.pop(previous_targets.index(int(target)))
                t.target_value = payload['value']
                t.note = payload['note']
                t.save()
        # create target
        else:
            if not payload['value'] == '':
                indicator = Indicator.objects.get(id=target)
                response = Target.objects.create(project=project,indicator=indicator,note=payload['note'],target_value=payload['value'])

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

    # Project filter
    if not request.GET['project_id'] == '0':
        project = Project.objects.filter(id=request.GET['project_id'])

        if request.GET['country'] == 'multi_country':
            project = Project.objects.filter(code=project[0].code,country__exclude=False)
    else:
        # Country filter
        if request.GET['country'] == 'all':
            project = Project.objects.filter(country__exclude=False)
        elif not request.GET['country'] == 'multi_country':
            project = Project.objects.filter(country__id = request.GET['country'],country__exclude=False)
        else:
            project = Project.objects.filter(country__exclude=False)


    if 'start_date' in request.GET and 'end_date' in request.GET:
        start_datetime = parse_datetime(request.GET['start_date']+' 00:00')
        start_date = parse_date(request.GET['start_date'])
        end_date = parse_date(request.GET['end_date'])
        today = datetime.date.today()

        if end_date > today:
            end_date = today
        programs = Program.objects.filter(project__in=project,end_date__gte = start_datetime,start_date__lte=end_date)
    # specific project, specific country
    else:
        # Me was born 08-04-1991
        start_datetime = datetime.datetime(1991,4,8,0,0,0)
        start_date = None
        end_date = datetime.date.today()
        # country and project filter to be done
        programs = Program.objects.filter(project__in=project)

    total_hours = 0
    total_responses = 0
    total_respondents = []
    percentage_better_gei = 0
    total_episodes_aired = 0
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
    avg_number_of_broadcast = 0

    weekdays = ( 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

    import json
    # import csv
    # response = HttpResponse(content_type='text/csv')
    # response['Content-Disposition'] = 'attachment; filename="Uliza-Log-export'
    # writer = csv.writer(response)
    # writer.writerow(['Radio series code','Public name','Radio station name','Project name','Country','Episodes aired','Episodes reviewed','Airtime (mins)','start date','end_date'])

    for program in programs:
        avg_number_of_broadcast +=1

        # number of episodes represent the episode number in the the time interval chosen
        number_of_episodes = 0

        # program_episodes_aired represent the # of episodes aired by this specific program
        program_episodes_aired = program.weeks_aired()
        episode_length_avg += program.duration
        start_week_number=0
        episodes_better_scored = 0
        end_week_number = program.weeks
        program.start_date = program.start_date.replace(tzinfo=None)

        if program.start_date <= start_datetime:
            week_diff = weeks_diff(program.start_date,start_datetime)

            start_week_number = week_diff[0]+1
            if program.start_date.weekday() <= start_datetime.weekday():
                start_week_number +=1


        if program.end_date >= end_date:
            week_diff = weeks_diff(end_date,program.end_date)
            end_week_number = week_diff[0]
            end_week_number = program.weeks-end_week_number

            if end_date.weekday()>= program.start_date.weekday():
                end_week_number +=1

        number_of_episodes = end_week_number - start_week_number
        postponements = Log.objects.filter(program=program,postpone=True,week__gte=start_week_number,week__lte=end_week_number)
        number_of_episodes = number_of_episodes - len(postponements)

        if program.repeat_start_time:
            avg_number_of_broadcast +=1
            duration = program.duration*2
        else:
            duration = program.duration

        if program_episodes_aired > number_of_episodes:
            # cut the program episodes aired number to the number within the time interval
            program_episodes_aired = number_of_episodes

        if number_of_episodes > 0:
            # todo there are times where there could be duplicate reviews for an episode
            reviews = Review.objects.filter(log__week__gte=start_week_number,log__week__lte=end_week_number,log__program=program).order_by('log__id')

            total_number_of_programs += 1
            start_week_number = int(start_week_number)
            start_week_number = int(start_week_number)
            episodes_better_scored = reviews.filter(numerical_score__gt=50)


            log_id = 0
            # note for country level stats: some reviews are duplicates
            for review in reviews:
                if log_id == review.log.id:
                    continue;
                else:
                    log_id = review.log.id
                if review.calculate_score(True) > 50:
                    percentage_better_gei += 1

                total_reviews += 1

            episodes_better_scored = len(episodes_better_scored)
            total_stations[program.radio_station.id] = program.radio_station.name

            if program.broadcast_language:
                total_languages[program.broadcast_language.id] = program.broadcast_language.name

            # add the program_episode_aired to the total_episodes_aired
            total_episodes_aired = total_episodes_aired + program_episodes_aired

            if program.implementation_type == 'impact':
                impact_stations[program.radio_station.id] = program.radio_station.name
            elif program.implementation_type == 'network':
                network_stations[program.radio_station.id] = program.radio_station.name

            # total_hours will get into negative
            if program.repeat_start_time and program.repeat_week_day:
                if weekdays.index(program.repeat_week_day) <= program.start_date.weekday() or weekdays.index(program.repeat_week_day) > end_date.weekday():
                    total_hours = total_hours - duration/2

            duration = duration*program_episodes_aired
            total_hours = total_hours + duration

            if 'export' in request.GET:
                try:
                    if not program.public_name:
                        public_name = ''
                    else:
                        public_name = program.public_name

                    writer.writerow([program.name.encode('utf8'),public_name.encode('utf8'),program.radio_station.name.encode('utf8'),program.project.name.encode('utf8'),program.project.country.name.encode('utf8'),int(number_of_episodes),len(reviews),duration,str(program.start_date),str(program.end_date)])
                except Exception as e:
                    pass

            if not program.poll_program_id:
                polls = PollSegment.objects.filter(program=program,episode_number__gte=start_week_number,episode_number__lte=end_week_number)
                polling_stats = RespondentStat.objects.filter(program=program,episode_number__gte=start_week_number,episode_number__lte=end_week_number)
                total_polls += len(polls)
                responses = polls.aggregate(Sum('number_of_responses'))

                if responses['number_of_responses__sum']:
                    total_responses += responses['number_of_responses__sum']

                for poll in polling_stats:
                    total_respondents = total_respondents + json.loads(poll.unique_respondents_list)

        total_better_episodes += episodes_better_scored

    # check the use of total_episodes_aired here
    if total_episodes_aired > 0:
        percentage_reviews = (float(total_reviews)/total_episodes_aired)*100
        percentage_reviews = math.floor(percentage_reviews)

    if total_better_episodes > 0:
        total_better_episodes = (float(total_better_episodes)/total_reviews)*100
        total_better_episodes = math.floor(total_better_episodes)

    if percentage_better_gei > 0:
        percentage_better_gei = (float(percentage_better_gei)/total_reviews)*100
        percentage_better_gei = math.floor(percentage_better_gei)


    total_hours = total_hours/60
    impact_stations = len(impact_stations)
    total_languages = len(total_languages)
    network_stations = len(network_stations)
    total_stations = len(total_stations)

    if avg_number_of_broadcast > 0:
        avg_number_of_broadcast = float(avg_number_of_broadcast)/len(programs)
        avg_number_of_broadcast = round(avg_number_of_broadcast,1)

        check_trailing_zero = str(avg_number_of_broadcast)
        check_trailing_zero = check_trailing_zero[len(check_trailing_zero)-1]
        if check_trailing_zero == '0':
            avg_number_of_broadcast = round(avg_number_of_broadcast)

    if total_number_of_programs > 0:
        episode_length_avg = episode_length_avg/total_number_of_programs

    avg_number_of_broadcast = round(avg_number_of_broadcast,1)
    total_hours = math.floor(total_hours)
    percentage_reviews = math.floor(percentage_reviews)

    indicators = Indicator.objects.exclude(grouping=6)
    project_indicators = Indicator.objects.filter(project_tied=request.GET['project_id'])
    indicators = indicators | project_indicators
    indicators = indicators.values()
    # targets = Target.objects.filter(project__id=project.id)
    total_respondents = set(total_respondents)
    total_respondents = len(total_respondents)

    results = {}

    if 'export' in request.GET:
		return response

    from django.contrib.humanize.templatetags.humanize import naturalday
    for i in indicators:
        project = programs.values_list('project__id',flat=True)
        t = Target.objects.filter(indicator__id=i['id'],project__id__in=project).order_by('last_updated_at')
        i['editing'] = False
        i['saving'] = False
        i['created_at'] = ''

        if t:
            # t = t[0]
            if t.last().last_updated_by:
                last_updated_by = t.last().last_updated_by.first_name+' '+t.last_updated_by.last_name
            else:
                last_updated_by = None

            if i['aggregation'] == 'sum':
                calc = Sum
            else:
                calc = Avg

            result  = {}
            i['target'] = t.aggregate(calc('target_value'))['target_value__'+i['aggregation']]
            i['total_result'] = t.aggregate(calc('value'))['value__'+i['aggregation']]

            if start_date:
                result = Report.objects.filter(target__in=t,report_date__gte=start_date,report_date__lte=end_date).aggregate(calc('value'))
                if not result['value__'+i['aggregation']]:
                    i['requested_result'] = 0
                    i['result'] = 0
                else:
                    i['requested_result'] = result['value__'+i['aggregation']]
                    i['result'] = result['value__'+i['aggregation']]
            else:
                i['requested_result'] = i['total_result']
                i['result'] = i['total_result']

            i['last_updated_at'] = naturalday(t.last().last_updated_at)
            i['last_updated_by'] = last_updated_by
        else:
            i['target'] = -1
            i['result'] = 0
            i['target_id'] = None
            i['last_updated_at'] = None


        results['#target_id_'+str(i['id'])] = i


    return JsonResponse({'programs':total_number_of_programs,
						'percentage_better_episodes':total_better_episodes,
                        'percentage_better_gei': percentage_better_gei,
						'percentage_reviews':percentage_reviews,
						'number_of_reviews':total_reviews,
                        'total_episodes_aired': int(total_episodes_aired),
						'total_stations': total_stations,
						'unknown': 'TBD',
                        'avg_number_of_broadcast': avg_number_of_broadcast,
						'total_polls':total_polls,
						'total_languages':total_languages,
						'total_hours':total_hours,
						'total_responses':total_responses,
						'total_respondents':total_respondents,
						'impact_stations':impact_stations,
						'network_stations':network_stations,
						'episode_length_avg':episode_length_avg,
                        'results': results},safe=False)
