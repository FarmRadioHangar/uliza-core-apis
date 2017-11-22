from log_app.models import Administrator,Presenter,Group_account

admins = Administrator.objects.all()
presenters = Presenter.objects.all()
groups = Group_account.objects.all()

for admin in admins:
	admin.language = 'en-US'
	print(admin.id)
	admin.save()

print('admin done')
for presenter in presenters:
	presenter.language = 'en-US'
	presenter.save()
print('presenter done')
for group in groups:
	group.language = 'en-US'
	group.save()
print('group done')

print('done')

