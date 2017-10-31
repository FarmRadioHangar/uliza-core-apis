from log_app.models import Knowledge_partner, Contact, Auth0User
from django.contrib.auth.models import User

from django.core.management.base import BaseCommand, CommandError
from django.utils import translation

class Command(BaseCommand):

	def handle(self, *args, **options):
		partners = Knowledge_partner.objects.all()
		
		for partner in partners:
			user = Auth0User.objects.create(username=partner.user.username, password=partner.user.password,
											email=partner.user.email, role='partner',is_super_user=partner.user.is_superuser,
											notify_on_log_create=True)

			contact = Contact.objects.create(user_id=user.id,first_name=partner.user.first_name,
											last_name=partner.user.last_name,job_title=partner.role,
											organization=partner.organization, country=partner.country,
											phone_number=partner.phone_number)