from log_app.models import *
import os


from django.core.management.base import BaseCommand, CommandError

class Command(BaseCommand):
	def add_arguments(self, parser):
		parser.add_argument('count',nargs='+',type=int)

	def handle(self,*args,**options):
		logs = Log.objects.exclude(recording_backup=None)
		last_index = len(logs)
		logs = logs[last_index-options['count'][0]:]

		from django.core.files import File

		for log in logs:
			try:
				os.path.isfile(log.recording_backup.path.encode('utf8'))
				if not os.path.isfile(log.recording_backup.path.encode('utf8')):
					continue
			except(OSError, ValueError) as e:
				print 'Failed to process file - log id '+str(log.id)


			print 'Processing: ['+str(log.id)+'] '+str(log.program.project.country.name)+' '+str(log.program.name)+' Episode '+str(log.week)
			log.blob_media_storage = File(log.recording_backup)
			# try:
			# 	os.unlink(log.recording_backup.path)
			# except(OSError,ValueError) as e:
			# 	print 'Failed to delete file - ['+str(log.id)+']'
			#
			# log.recording_backup = None
			log.save()
