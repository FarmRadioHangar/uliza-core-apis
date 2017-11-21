from log_app.models import Log
import os

logs = Log.objects.filter(postpone=False,program__radio_station__country__name='Tanzania')
total = 0
for instance in logs:
	
	if (instance.recording_backup):
		try:
			size = instance.recording_backup.size

			if size > 25000000:
				print str(instance.program.name)+' '+str(instance.id)
		except OSError:
			pass
		except ValueError:
			success = False

print('done')