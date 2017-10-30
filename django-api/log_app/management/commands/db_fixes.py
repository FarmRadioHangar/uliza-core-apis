from log_app.models import Log
import os

logs = Log.objects.filter(postpone=False,program__radio_station__country__name='Ethiopia')
total = 0
for instance in logs:
	# prestenter field changed to user field
	# ----------------------------------------
	# if log.presenter:
	# 	log.saved_by = log.presenter.user
	# 	log.presenter = None
	# 	print(log.id)
	# 	log.save()
	# return HttpResponse('<h2>400 - Log not found</h2>',status=400)	
	if (instance.recording and instance.recording_backup):
		try:
			os.unlink( instance.recording_backup.path )
			instance.recording_backup = None
			instance.save()
			print str(instance.program.name)+' '+str(instance.week)
		except OSError:
			pass
		except ValueError:
			success = False

print('done')