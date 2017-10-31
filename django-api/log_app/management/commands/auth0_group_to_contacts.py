from log_app.models import Group_account, Contact, Auth0User
from django.contrib.auth.models import User

from django.core.management.base import BaseCommand, CommandError
from django.utils import translation

class Command(BaseCommand):

	def handle(self, *args, **options):
		groups = Group_account.objects.all()
		
		for group in groups:
			user = Auth0User.objects.create(username=group.user.username, password=group.user.password,
											email=group.user.email, role='group',is_super_user=group.user.is_superuser,
											notify_on_log_create=False)

			contact = Contact.objects.create(user_id=user.id,first_name=group.user.first_name,
											last_name=group.user.last_name,job_title="Group account for "+str(group.radio_station.name),
											organization=group.radio_station.name, country=group.radio_station.country,
											phone_number='')

			# the radio station model will hold the contact's id
			# on create the organization name will be duplicated here [in the organization attribute]