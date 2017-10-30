from log_app.models import Administrator

admins = Administrator.objects.all()

for admin in admins:
	if admin.notify_log_submission:
		admin.notify_log_submission = False
		print(admin.id)
		admin.save()
print('done')