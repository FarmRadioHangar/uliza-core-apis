from log_app.models import *
import os
from api_core.settings import BASE_DIR


from django.core.management.base import BaseCommand, CommandError

class Command(BaseCommand):
	def add_arguments(self, parser):
		parser.add_argument('count',nargs='+',type=int)

	def handle(self,*args,**options):
		logs = Log.objects.exclude(recording_backup='')
		logs = logs[:options['count'][0]]

		from django.core.files import File
		result = open(BASE_DIR+'/migration_log.txt','a')

		for log in logs:
			try:
				os.path.isfile(log.recording_backup.path.encode('utf8'))
				if not os.path.isfile(log.recording_backup.path.encode('utf8')):
					continue
			except(OSError, ValueError) as e:
				print 'Failed to process file - log id '+str(log.id)
				result.write('Failed to process file  - ['+str(log.id)+']\n')


			print 'Processing: ['+str(log.id)+'] '+str(log.program.project.country.name)+' '+str(log.program.name)+' Episode '+str(log.week)
			log.blob_media_storage = File(log.recording_backup)
			log.save()

			if log.blob_media_storage.url:
				try:
					os.unlink(log.recording_backup.path)
					log.recording_backup = None
					result.write('Successfully migrated - ['+str(log.id)+']\n')
				except(OSError,ValueError) as e:
					print 'Failed to delete file - ['+str(log.id)+']\n'
					result.write('Failed to delete file - ['+str(log.id)+']\n')
			log.save()

		result.close()
