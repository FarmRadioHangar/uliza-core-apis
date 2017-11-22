from log_app.models import Administrator, Contact, Auth0User
from django.contrib.auth.models import User

from django.core.management.base import BaseCommand, CommandError
from django.utils import translation

class Command(BaseCommand):

	def handle(self, *args, **options):
		admins = Administrator.objects.all()
		
		for admin in admins:
			user = Auth0User.objects.create(username=admin.user.username, password=admin.user.password,
											email=admin.user.email, role='admin',is_super_user=admin.user.is_superuser,
											notify_on_log_create=admin.notify_log_submission)

			contact = Contact.objects.create(user_id=user.id,first_name=admin.user.first_name,
											last_name=admin.user.last_name,job_title=admin.job_description,
											organization="Farm Radio International", country=admin.country,
											phone_number="")