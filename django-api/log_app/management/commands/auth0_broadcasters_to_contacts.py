from log_app.models import Presenter, Contact, Auth0User
from django.contrib.auth.models import User

from django.core.management.base import BaseCommand, CommandError
from django.utils import translation

class Command(BaseCommand):

	def handle(self, *args, **options):
		broadcasters = Presenter.objects.all()
		
		for broadcaster in broadcasters:
			user = Auth0User.objects.create(username=broadcaster.user.username, password=broadcaster.user.password,
											email=broadcaster.user.email, role='broadcaster',is_super_user=broadcaster.user.is_superuser,
											notify_on_log_create=False)

			contact = Contact.objects.create(user_id=user.id,first_name=broadcaster.user.first_name,
											last_name=broadcaster.user.last_name,job_title=broadcaster.role,
											organization=broadcaster.radio_station.name, country=broadcaster.radio_station.country,
											phone_number=broadcaster.phone_number)

			# the radio station model will hold the contact's id
			# on create the organization name will be duplicated here [in the organization attribute]