from django.http import JsonResponse
from rest_framework.decorators import api_view
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import PollSegment,Program
from log_app.serializers import PollSegmentSerializer
from rest_framework.authentication import get_authorization_header
from django.views.decorators.http import require_POST
from django.http import HttpResponse

from jfu.http import upload_receive, UploadResponse, JFUResponse
import requests
from api_core import settings

import django_filters
from rest_framework import filters

class PollSegmentGet(generics.ListCreateAPIView):
    queryset = PollSegment.objects.all()
    model = PollSegment
    serializer_class = PollSegmentSerializer
    filter_backends = (filters.OrderingFilter,filters.DjangoFilterBackend)
    filter_fields = ['id','program','episode_number','index']
    ordering_fields = ['id','index']

    def get_queryset(self):
        last_poll = self.request.GET.get('last')
        highest_respondent = self.request.GET.get('highest_respondent')
        program = self.request.GET.get('program')
        episode_number = self.request.GET.get('episode_number')
        if last_poll and program:
            instance = PollSegment.objects.filter(program=program).last()
            if instance:
                queryset = PollSegment.objects.filter(id=instance.id)
            else:
                queryset = PollSegment.objects.none()
        elif highest_respondent and program and episode_number:
            instance = PollSegment.objects.filter(program=program,episode_number=episode_number).order_by('-number_of_respondents')
            if instance:
                queryset = instance.filter(id = instance[0].id)
            else:
                queryset = PollSegment.objects.none()
        else:
            queryset = PollSegment.objects.all()

        return queryset

class PollSegmentEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = PollSegment.objects.all()
    model = PollSegment
    serializer_class = PollSegmentSerializer
    lookup_field = 'id'

@api_view(['GET'])
def last_entry(request,id):
    last_entry = PodDistributionLog.objects.filter(podcast = id).last()

    if last_entry:
        last_entry = PodDistributionLogSerializer(last_entry)
        return JsonResponse(last_entry.data)
    else:
        return JsonResponse({})


@require_POST
def upload( request ):
    pollsegment_id = request.POST['log_id']
    file = upload_receive( request )

    import os,re

    instance = PollSegment.objects.get(pk=pollsegment_id)
    if instance.poll_file:
        basename = os.path.basename( instance.poll_file.path )
    else:
        basename = ''

    instance.save()
    pollsegment_id = str(instance.id)
    file.name = file.name.encode('ascii','ignore')
    filename = pollsegment_id+'_'+re.sub("[^\w.-]", '', file.name.replace(" ","_"))
    if(not basename == filename):
        if(instance.poll_file):
            try:
                os.unlink( instance.poll_file.path )
            except OSError as e:
                pass

        file.name = pollsegment_id+'_'+file.name
        instance.public = False
        instance.poll_file = file
        basename = os.path.basename( instance.poll_file.path )
        instance.poll_file_offset = file.size
    else:
        instance.append_chunk(file,file.size,False)

    content_range_header = 'HTTP_CONTENT_RANGE'
    content_range_pattern = re.compile(
	    r'^bytes (?P<start>\d+)-(?P<end>\d+)/(?P<total>\d+)$'
	)

    content_range = request.META.get(content_range_header, '')
    match = content_range_pattern.match(content_range)

    if not content_range == '':
        total = int(match.group('total'))
    else:
        total = file.size

    if(total == instance.poll_file.size):
        instance.poll_file_saved = True
        instance.save()
        instance.rename()
    else:
        instance.poll_file_saved = False
        instance.save()

    file_dict = {
	    'filename' : basename,
	    'length' : instance.poll_file.size,

	    'url': instance.poll_file.path,
	    'thumbnailUrl': settings.MEDIA_URL + basename,
	    '_id': instance.pk,

	    'deleteUrl': 'http://'+request.get_host()+settings.SUB_SITE+settings.LINK+'/podcasts/recording/delete/'+str(instance.id),
	    'deleteType': "POST",
	}

    return UploadResponse( request, file_dict )


@require_POST
def upload_delete( request, pk ):
    success = True
    try:
    	import os
        instance = PollSegment.objects.get( pk = pk )
        os.unlink( instance.poll_file.path )
        instance.poll_file = None
        instance.save()
    except ValueError:
        success = False

    return JFUResponse( request, success )


def check_file(request,poll_segment_id,filename):
	import os.path,re

	filename = filename.encode('ascii','ignore')
	filename = re.sub("[^\w.-]", '', filename.replace(" ","_"))
	filepath = settings.MEDIA_ROOT+'/'+poll_segment_id+'_'+filename

	if(os.path.isfile(filepath)):
		return HttpResponse(os.path.getsize(filepath))

	return JFUResponse( request, False )

def find(pattern,path):
    import os,fnmatch
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                result.append(os.path.join(root, name))
    return result


def extract_data(request,poll_segment_id):
    poll_segment = PollSegment.objects.get(pk=poll_segment_id)

    try:
        import shutil
        shutil.rmtree(settings.MEDIA_ROOT+'/'+poll_segment_id)
    except OSError as e:
        pass

    import zipfile, re
    data = zipfile.ZipFile(poll_segment.poll_file.path,'r')
    data.extractall(settings.MEDIA_ROOT+'/extracted_polls/'+poll_segment_id)

    finder = find('details.txt',settings.MEDIA_ROOT+'/extracted_polls/'+poll_segment_id)

    data = []
    audio_ids = []
    total_responses = 0

    for detail in finder:
        info = open(detail,'r')

        snippet = info.read()
        detail = {}

        if snippet:
            total_responses += 1
            detail['audio_file_id'] = re.findall(r'Audio file ID: (.+)\n',snippet)
            if detail['audio_file_id'][0] in audio_ids:
                continue;
            else:
                audio_ids.append(detail['audio_file_id'][0])

            detail['survey_name'] = re.findall(r'Survey Name:(.+)\n',snippet)
            detail['survey_name'] = detail['survey_name'][0].strip()
            detail['question_name'] = re.findall(r'Question Name:(.+)\n',snippet)
            detail['question_name'] = detail['question_name'][0].strip()
            detail['phone'] = re.findall(r'Phone:(.+)\n',snippet)
            detail['phone'] = detail['phone'][0].strip()
            detail['subscriber_name'] = re.findall(r'Subscriber Name:(.+)\n',snippet)
            detail['subscriber_name'] = detail['subscriber_name'][0].strip()
            detail['recording_date'] = re.findall(r'Recording Date:(.+)\n',snippet)
            detail['recording_date'] = detail['recording_date'] [0].strip()
            detail['poll_id'] = re.findall(r'Poll ID:(.+)\n',snippet)
            detail['poll_id'] = detail['poll_id'][0].strip()
            detail['audio_file_id'] = re.findall(r'Audio file ID:(.+)\n',snippet)
            detail['audio_file_id'] = detail['audio_file_id'][0].strip()
            detail['trimmed_audio_length'] = re.findall(r'Trimmed audio length:(.+)seconds',snippet)
            detail['trimmed_audio_length'] = detail['trimmed_audio_length'][0].strip()
            detail['original_audio_length'] = re.findall(r'Original audio length:(.+)seconds',snippet)
            detail['original_audio_length'] = detail['original_audio_length'] [0].strip()
            detail['audio_url'] = re.findall(r'Audio URL:(.+)\n',snippet)
            detail['audio_url'] = detail['audio_url'][0].strip()

            data.append(detail)

        info.close()

    import json
    if data:
        poll_segment.title = data[0]['question_name']
        poll_segment.number_of_responses = total_responses
        poll_segment.result = json.dumps(data)
        poll_segment.save()

    return JsonResponse(data,safe=False)

def create_instance(request,index,episode_number,program_id):
	try:
		program = Program.objects.get(pk=program_id)
	except Exception, e:
		request.session['error_msg'] = 'Access error'

        # not returning proper response is BAD
		# return HttpResponse('false')

	instance = PollSegment(program=program,index=index,episode_number=episode_number,type='open')
	instance.save()

	return HttpResponse(instance.id)
