from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Log, Program
from log_app.serializers import LogSerializer

from rest_framework import filters
from rest_framework.pagination import PageNumberPagination
from django_filters.rest_framework import DjangoFilterBackend

from django.views.decorators.http import require_POST
from django.views.decorators.csrf import csrf_exempt

import django_filters
from rest_framework import filters

from django.contrib.syndication.views import Feed
from django.utils.feedgenerator import Rss201rev2Feed
from django.core.urlresolvers import reverse

from django.views.decorators.csrf import csrf_exempt
from django.core.files.base import ContentFile

from api_core import settings
from jfu.http import upload_receive, UploadResponse, JFUResponse
from django.shortcuts import redirect
from django.contrib.sites.models import Site
from django.http import HttpResponse

class iTunesFeed(Rss201rev2Feed):
    def rss_attributes(self):
        return {
            "version": self._version,
            "xmlns:atom": "http://www.w3.org/2005/Atom",
            'xmlns:itunes': u'http://www.itunes.com/dtds/podcast-1.0.dtd'
        }

    def add_root_elements(self, handler):
        super(iTunesFeed, self).add_root_elements(handler)
        handler.addQuickElement('itunes:author', self.feed['author_name'])
        handler.addQuickElement('itunes:summary', self.feed['description'])
        handler.addQuickElement('itunes:category', 'education')
        handler.addQuickElement('itunes:explicit', 'clean')
        handler.startElement("itunes:owner", {})
        handler.addQuickElement('itunes:name', self.feed['author_name'])
        handler.addQuickElement('itunes:email', self.feed['itunes_email'])
        handler.endElement("itunes:owner")
        handler.addQuickElement('itunes:image', settings.DEFAULT_PODCAST_IMAGE)

    def add_item_elements(self, handler, item):
        super(iTunesFeed, self).add_item_elements(handler, item)
        handler.addQuickElement(u'itunes:summary', item['summary'])
        # handler.addQuickElement(u'itunes:duration', item['duration'])
        handler.addQuickElement(u'itunes:explicit', 'clean')

class ProgramLogFeed(Feed):
    feed_type = iTunesFeed
    link = "/api/v1/"
    domain = Site.objects.get_current().domain

    def feed_extra_kwargs(self, obj):
        return {'itunes_email': obj.radio_station.email}

    def item_extra_kwargs(self, item):
        return {'summary': item.focus_statement}

    def get_object(self,request,program_id):
        return Program.objects.get(id=program_id)

    def title(self,obj):
        if obj.public_name:
            return obj.name+': '+str(obj.public_name)
        else:
            return obj.name

    def author_name(self,obj):
        return obj.radio_station.name

    def description(self,obj):
        return obj.project.focus

    def item_author_name(self,item):
        return item.program.radio_station.name

    def item_author_email(self,item):
        return item.program.radio_station.email

    def items(self,obj):
        return Log.objects.filter(program__id=obj.id)

    def item_title(self, item):
        return item.topic

    def item_enclosure_url(self, item):
        if item.recording_backup:
            return 'https://'+self.domain+item.recording_backup.url
        else:
            return ''

    def item_description(self, item):
        return item.focus_statement

    def item_pubdate(self,item):
        return item.created_at

    def item_link(self, item):
        if item.recording_backup:
            return 'https://'+self.domain+item.recording_backup.url
        else:
            return ''

    def item_enclosure_mime_type(self, item):
        return 'audio/mp3'

class LargeResultsSetPagination(PageNumberPagination):
    page_size = 1000
    page_size_query_param = 'page_size'
    max_page_size = 10000

class LogFilter(filters.FilterSet):
	week__lte = django_filters.NumberFilter(name="week", lookup_expr='lte')
	id__lt = django_filters.DateTimeFilter(name="id", lookup_expr='lt')
	country__not = django_filters.NumberFilter(name="program__radio_station__country", exclude=True)
	program__radio_station = django_filters.NumberFilter(name="program__radio_station")

	class Meta:
		model = Log
		fields = ['id','week','program','program__radio_station__country','postpone','week__lte','id__lt','saved_by','formats']

class LogGet(generics.ListCreateAPIView):

	model = Log
	serializer_class = LogSerializer
	filter_class = LogFilter

	pagination_class = LargeResultsSetPagination
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id','created_at','postpone')
	fields = ['id']

	def get_queryset(self):

		queryset = Log.objects.all().select_related('program__project__id','program','program__radio_station__country__id','program__radio_station__name','program__radio_station__id','program__start_date').prefetch_related('formats')

		pk_list = self.request.GET.get('program__in')
		if pk_list:
			pk_list = pk_list.split(',')
			logs = queryset.filter(program__in=pk_list)
		else:
			logs = queryset

		pk_list = self.request.GET.get('project__in')
		if pk_list:
			pk_list = pk_list.split(',')
			logs = queryset.filter(program__project__in=pk_list)
		else:
			logs = queryset


		return logs

class LogEntity(generics.RetrieveUpdateDestroyAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    lookup_field = 'id'

    def perform_destroy(self, instance):
		try:
			import os
			os.unlink( instance.recording_backup.path )
			instance.recording_backup = None
			instance.save()
		except (OSError,ValueError) as e:
		    success = False

		if (instance.postpone):
			import datetime
			weeks = datetime.timedelta(weeks = 1)
			instance.program.end_date = instance.program.end_date - weeks
			instance.program.save()

		instance.delete()



@require_POST
def upload( request ):
	log_id = request.POST['log_id']
	file = upload_receive( request )

	import os,re

	instance = Log.objects.get(pk=log_id)
	if instance.recording_backup:
		basename = os.path.basename( instance.recording_backup.path )
	else:
		basename = ''

	instance.save()
	log_id = str(instance.id)
	file.name = file.name.encode('ascii','ignore')
	filename = log_id+'_'+re.sub("[^\w.-]", '', file.name.replace(" ","_"))
	if(not basename == filename):
		if(instance.recording_backup):
			try:
				os.unlink( instance.recording_backup.path )
			except OSError as e:
				pass

		file.name = log_id+'_'+file.name
		instance.recording_backup = file
		basename = os.path.basename( instance.recording_backup.path )
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

	if(total == instance.recording_backup.size):
		instance.recording_saved = True
		instance.rename()
	else:
		instance.recording_saved = False

	instance.recording = None
	instance.save()

	file_dict = {
	    'filename' : basename,
	    'length' : instance.recording_backup.size,

	    'url': instance.recording_backup.path,
	    'thumbnailUrl': settings.MEDIA_URL + basename,
	    '_id': instance.pk,

	    'deleteUrl': settings.SUB_SITE+'/logs/recording/delete/'+str(instance.id),
	    'deleteType': "POST",
	}

	return UploadResponse( request, file_dict )

@require_POST
def upload_delete( request, pk ):
    success = True
    try:
    	import os
        instance = Log.objects.get( pk = pk )
        os.unlink( instance.recording_backup.path )
        instance.recording_backup = None
        instance.save()
    except ValueError:
        success = False

    return JFUResponse( request, success )

def open_with_drive(request,pk):
    log = Log.objects.get(pk=pk)

    if(log.gdrive_available):
        # get the old link from dev api
        import requests
        response = requests.get('https://dev.uliza.fm/api/v1/logs/recording/gdrive/'+str(pk),params={})
        if response.status_code == 200:
            return redirect(response.content)
        else:
            return HttpResponse('<h2>404 Not found</h2>',status=404)

    elif(log.gdrive):
        return redirect(log.gdrive_url)
    elif(log.recording_backup):
        # Uploading to gdrive
        import os
        from django.core.files import File
        log.gdrive = File(log.recording_backup,log.program.name+'_week_'+str(log.week)+'.mp3')
        log.save()

        log.gdrive_url = log.gdrive.url


        if 'archive' in request.GET:
            os.unlink( log.recording_backup.path )
            log.recording_backup = None

        log.save()

    	return redirect(log.gdrive_url)

    return HttpResponse('<h2>404 Not found</h2>',status=404)

def rec_download(request,pk):
	log = Log.objects.get(pk=pk)

	if(log.recording_backup):
		import os

		basename = os.path.basename(log.recording_backup.url)
		download = '/media/'+str(basename)

		return redirect(download)
	else:
		return HttpResponse('<h2>404</h2>')


def check_rec(request,log_id,filename):
	import os.path,re

	filename = filename.encode('ascii','ignore')
	filename = re.sub("[^\w.-]", '', filename.replace(" ","_"))
	filepath = settings.MEDIA_ROOT+'/'+log_id+'_'+filename

	if(os.path.isfile(filepath)):
		return HttpResponse(os.path.getsize(filepath))

	return JFUResponse( request, False )

def create_instance(request,week,program_id):
	from log_app.models import Program

	try:
		program = Program.objects.get(pk=program_id)
	except Exception, e:
		request.session['error_msg'] = 'Access error'
		return False

	instance = Log(topic='',program_id=program.id,week=week)
	instance.save()

	return HttpResponse(instance.id)
