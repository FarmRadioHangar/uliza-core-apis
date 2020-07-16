from log_app.models import *
import os

"""
# of radio broadcasts archived on Uliza Log
# of radio campaigns created in the FY
# of comments exchanged between broadcasters and trainers on Uliza Log
# of knowledge parters who had access to active programs on Uliza log in the FY
# of knowledge partners who joined in the FY
# of knowledge partners participating on Uliza Log
# of comments contributed by knowledge partners on Uliza Log
"""

from django.core.management.base import BaseCommand, CommandError
# of radio broadcasts archived on Uliza Log
# comment this is number of logs
class Command(BaseCommand):
	def handle(self,*args,**options):

		from datetime import datetime
		from django.db.models import Count

		start_date = datetime(2019,4,1)
		end_date = datetime(2020,3,31)
		broadcasts = Log.objects.filter(created_at__gte=start_date,created_at__lte=end_date)

		campaigns = Program.objects.filter(created_at__gte=start_date, created_at__lte=end_date)

		comments = Comment.objects.filter(created_at__gte=start_date,created_at__lte=end_date).exclude(contact__role="knowledge_partner").exclude(contact__role='gender_specialist')

		# kp who had access to active programs
		kp_access = Program.objects.filter(end_date__gt=start_date,access__role='knowledge_partner').values('access').annotate(total=Count('access'))

		new_kp = Contact.objects.filter(created_at__gte=start_date, created_at__lte = end_date,role='knowledge_partner')

		kp_comments = Comment.objects.filter(created_at__gte=start_date,created_at__lte=end_date).filter(contact__role='knowledge_partner')

		kp_participants = kp_comments.values('contact__id').annotate(total=Count('contact__id'))

		print('# of radio broadcasts archived ',len(broadcasts))
		print('# of radio campaigns created ', len(campaigns))
		print('# of comments exchanged between broadcasters and trainers', len(comments))
		print('# of knowledge partners who had access to active programs',len(kp_access))
		print('# of knowledge partners who joined in the FY',len(new_kp))
		print('# of knowledge partners participating on Uliza log',len(kp_participants))
		print('# of comments contributed by knowledge partners partners',len(kp_comments))
