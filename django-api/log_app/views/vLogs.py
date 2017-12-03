from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Log
from log_app.serializers import LogSerializer

from rest_framework import filters
from rest_framework.pagination import PageNumberPagination
from django_filters.rest_framework import DjangoFilterBackend

from django.views.decorators.http import require_POST
from django.views.decorators.csrf import csrf_exempt

import django_filters
from rest_framework import filters

class LargeResultsSetPagination(PageNumberPagination):
	page_size = 1000
	page_size_query_param = 'page_size'
	max_page_size = 10000

class LogFilter(filters.FilterSet):
	week__lte = django_filters.DateTimeFilter(name="week", lookup_expr='lte')
	id__lt = django_filters.DateTimeFilter(name="id", lookup_expr='lt')

	class Meta:
		model = Log
		fields = ['id','week','program','program__radio_station__country','postpone','week__lte','id__lt']

class LogGet(generics.ListCreateAPIView):

	queryset = Log.objects.all()

	model = Log
	serializer_class = LogSerializer
	filter_class = LogFilter

	pagination_class = LargeResultsSetPagination
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id','created_at')
	fields = ['id']

	def get_queryset(self):
		"""
		This view should return a list of all the purchases
		for the currently authenticated user.
		"""

		queryset = Log.objects.all().select_related('program__project__id','program','program__radio_station__country__id','program__radio_station__name','program__radio_station__id','program__start_date')

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


class LogEntity(generics.RetrieveUpdateAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    lookup_field = 'id'



from django.views.decorators.csrf import csrf_exempt
from django.core.files.base import ContentFile

from api_core import settings
from jfu.http import upload_receive, UploadResponse, JFUResponse

from django.http import HttpResponse

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
	filename = log_id+'_'+re.sub("[^\w.-]", '', file.name.replace(" ","_"))
	if(not  basename == filename):
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

	if(log.recording):
		return redirect(log.recording.url)
	elif(log.recording_backup):
		# Uploading to gdrive
		import os
		from django.core.files import File
		log.recording = File(log.recording_backup,log.program.name+'_week_'+str(log.week)+'.mp3')
		log.save()

		return redirect(log.recording.url)

	return HttpResponse('<h2>404 Not found</h2>',status=404)

def rec_download(request,pk):
	log = Log.objects.get(pk=pk)

	if(log.recording_backup):
		import os

		basename = os.path.basename(log.recording_backup.url)
		download = '/media/'+str(basename)

		return redirect(download)

		# response = HttpResponse(content_type = mimetype, status=206)
		# response['Content-Length'] = os.path.getsize(download)
		# response['Content-Disposition'] = 'attachment; filename='+str(basename)

		return response
	else:
		return HttpResponse('<h2>404</h2>')


def check_rec(request,log_id,filename):
	import os.path,re

	filename = re.sub("[^\w.-]", '', filename.replace(" ","_"))
	filepath = settings.MEDIA_ROOT+log_id+'_'+filename

	if(os.path.isfile(filepath)):
		return HttpResponse(os.path.getsize(filepath))

	return JFUResponse( request, False )

def create_instance(request,week,program_id):
	# This should be replaced by auth0
	# if (request.user.is_admin()):
	# 	user = Administrator.objects.get(user__id = request.user.id)
	# 	user = user.user
	# 	a_programs = Program.objects.all()
	# else:
	# 	presenter = Presenter.objects.filter(user__id = request.user.id)
	# 	if presenter:
	# 		user = presenter[0].user
	# 	else:
	# 		group = Group_account.objects.get(user__id = request.user.id)
	# 		user = group.user

	# 	a_programs = Program.objects.filter(access = user)
	from log_app.models import Program

	try:
		program = Program.objects.get(pk=program_id)
	except Exception, e:
		request.session['error_msg'] = 'Access error'
		return False

	instance = Log(topic='',program_id=program.id,week=week)
	instance.save()

	return HttpResponse(instance.id)

def delete( request, pk ):
	# return HttpResponse('<h2>400 - Log not found</h2>',status=400)
	admin = request.user.is_admin()
	instance = Log.objects.get(pk = pk)
	if(not instance):
		return HttpResponse('<h2>400 - Log not found</h2>',status=400)

	if(not admin and not instance.postpone):
		return HttpResponse('<h2>403 - Forbidden</h2>',status=403)

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

	return HttpResponseRedirect(request.META.get('HTTP_REFERER','/'))
