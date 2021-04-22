from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Podcast,PodEpisode,PodDistributionLog
from log_app.serializers import PodcastSerializer
from django.views.decorators.http import require_POST
from django.http import HttpResponse
from django.db.models import Q

from api_core import settings
from jfu.http import upload_receive, UploadResponse, JFUResponse

import django_filters
from rest_framework import filters

class PodcastGet(generics.ListCreateAPIView):

    queryset = Podcast.objects.all()
    model = Podcast
    serializer_class = PodcastSerializer
    filter_fields = ['id','radio_station','spreaker_show_id','spotify_status','podcast_addict_status','amazon_music_status','apple_podcasts_status','google_podcasts_status']

    def get_queryset(self):
        status = self.request.GET.get('distribution_status')
        if status:
            if status[0] == '-':
                queryset = Podcast.objects.exclude(Q(spotify_status=status[1:])|
                                                  Q(google_podcasts_status=status[1:])|
                                                  Q(podcast_addict_status=status[1:])|
                                                  Q(amazon_music_status=status[1:])|
                                                  Q(apple_podcasts_status=status[1:]))
            else:
                queryset = Podcast.objects.filter(Q(spotify_status=status)|
                                                  Q(google_podcasts_status=status)|
                                                  Q(podcast_addict_status=status)|
                                                  Q(amazon_music_status=status)|
                                                  Q(apple_podcasts_status=status))
        else:
            queryset = Podcast.objects.all().select_related('radio_station__name')

        return queryset

class PodcastEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Podcast.objects.all()
    model = Podcast
    serializer_class = PodcastSerializer
    lookup_field = 'id'

    def perform_update(self,serializer):
        original_instance = Podcast.objects.get(id=self.request._full_data['id'])
        instance = serializer.save()

        if 'description' in self.request._full_data:
            description = self.request._full_data['description']
        else:
            description = ''

        if original_instance.apple_podcasts_status == instance.apple_podcasts_status:
            instance.apple_podcasts_status = None

        if original_instance.spotify_status == instance.spotify_status:
            instance.spotify_status = None

        if original_instance.google_podcasts_status == instance.google_podcasts_status:
            instance.google_podcasts_status = None

        if original_instance.podcast_addict_status == instance.podcast_addict_status:
            instance.podcast_addict_status = None

        if original_instance.amazon_music_status == instance.amazon_music_status:
            instance.amazon_music_status = None

        PodDistributionLog.objects.create(podcast=instance,
                                          triggered_by_id=self.request._full_data['contact'],
                                          note=description,
                                          apple_podcasts_status= instance.apple_podcasts_status,
                                          spotify_status= instance.spotify_status,
                                          google_podcasts_status= instance.google_podcasts_status,
                                          podcast_addict_status= instance.podcast_addict_status,
                                          amazon_music_status= instance.amazon_music_status)
@require_POST
def upload( request ):
	episode_id = request.POST['log_id']
	file = upload_receive( request )

	import os,re

	instance = PodEpisode.objects.get(pk=episode_id)
	if instance.audio_file:
		basename = os.path.basename( instance.audio_file.path )
	else:
		basename = ''

	instance.save()
	episode_id = str(instance.id)
	file.name = file.name.encode('ascii','ignore')
	filename = episode_id+'_'+re.sub("[^\w.-]", '', file.name.replace(" ","_"))
	if(not basename == filename):
		if(instance.audio_file):
			try:
				os.unlink( instance.audio_file.path )
			except OSError as e:
				pass

		file.name = episode_id+'_'+file.name
		instance.public = False
		instance.audio_file = file
		basename = os.path.basename( instance.audio_file.path )
		instance.offset = file.size
	else:
		instance.append_chunk(file,file.size,False)

	content_range_header = 'HTTP_CONTENT_RANGE'
	content_range_pattern = re.compile(
	    r'^bytes (?P<start>\d+)-(?P<end>\d+)/(?P<total>\d+)$'
	)

	content_range = request.META.get(content_range_header, '')
	match = content_range_pattern.match(content_range)

	total = int(match.group('total'))

	if(total == instance.audio_file.size):
		instance.audio_saved = True
		instance.rename()
	else:
		instance.audio_saved = False

	instance.recording = None
	instance.save()

	file_dict = {
	    'filename' : basename,
	    'length' : instance.audio_file.size,

	    'url': instance.audio_file.path,
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
        instance = PodEpisode.objects.get( pk = pk )
        os.unlink( instance.audio_file.path )
        instance.audio_file = None
        instance.save()
    except ValueError:
        success = False

    return JFUResponse( request, success )

def rec_download(request,pk):
	log = Log.objects.get(pk=pk)

	if(log.recording_backup):
		import os

		basename = os.path.basename(log.recording_backup.url)
		download = '/media/'+str(basename)

		return redirect(download)
	else:
		return HttpResponse('<h2>404</h2>')


def check_rec(request,episode_id,filename):
	import os.path,re

	filename = filename.encode('ascii','ignore')
	filename = re.sub("[^\w.-]", '', filename.replace(" ","_"))
	filepath = settings.MEDIA_ROOT+'/'+episode_id+'_'+filename

	if(os.path.isfile(filepath)):
		return HttpResponse(os.path.getsize(filepath))

	return JFUResponse( request, False )

def create_instance(request,podcast_number,podcast_id):
	try:
		podcast = Podcast.objects.get(pk=podcast_id)
	except Exception, e:
		request.session['error_msg'] = 'Access error'
		return False

	instance = PodEpisode(podcast_id=podcast.id)
	instance.save()

	return HttpResponse(instance.id)
