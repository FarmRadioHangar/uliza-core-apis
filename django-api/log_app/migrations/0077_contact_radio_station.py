# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def change_accounts_to_contacts(apps,schema_editor):
    Contact = apps.get_model('log_app', 'Contact')
    Auth0User = apps.get_model('log_app', 'Auth0User')
    Administrator = apps.get_model('log_app', 'Administrator')
    Group_account = apps.get_model('log_app', 'Group_account')
    Presenter = apps.get_model('log_app', 'Presenter')
    Knowledge_partner = apps.get_model('log_app', 'Knowledge_partner')

    admins = Administrator.objects.all()
    email = ''
    for admin in admins:
        if admin.user.email:
            email = admin.user.email
        else:
            email = admin.user.username+'@uliza_log.fm'

        user = Auth0User.objects.create(username=admin.user.username, password=admin.user.password,
        								email=email, role='staff',is_super_user=admin.user.is_superuser,
        								notify_on_log_create=admin.notify_log_submission)

        contact = Contact.objects.create(user_id='auth0|'+str(user.id),first_name=admin.user.first_name,
    									last_name=admin.user.last_name,job_title=admin.job_description,
    									organization="Farm Radio International", country_id=admin.country.id,
    									phone_number="",role='staff')

    broadcasters = Presenter.objects.all()
    for broadcaster in broadcasters:
        if broadcaster.user.email:
            email = broadcaster.user.email
        else:
            email = broadcaster.user.username+'@uliza_log.fm'

    	user = Auth0User.objects.create(username=broadcaster.user.username, password=broadcaster.user.password,
    									email=email, role='broadcaster',is_super_user=broadcaster.user.is_superuser,
    									notify_on_log_create=False)

    	contact = Contact.objects.create(user_id='auth0|'+str(user.id),first_name=broadcaster.user.first_name,
    									last_name=broadcaster.user.last_name,job_title=broadcaster.role,
    									organization=broadcaster.radio_station.name, country_id=broadcaster.radio_station.country.id,
    									phone_number=broadcaster.phone_number, radio_station=broadcaster.radio_station.id,role='broadcaster')

    	# the radio station model will hold the contact's id
    	# on create the organization name will be duplicated here [in the organization attribute]

    groups = Group_account.objects.all().order_by('-user__last_login')
    for group in groups:
        if group.user.email:
            email = group.user.email
        else:
            email = group.user.username+'@uliza_log.fm'
    	user = Auth0User.objects.create(username=group.user.username, password=group.user.password,
    									email=email, role='group',is_super_user=group.user.is_superuser,
    									notify_on_log_create=False)

    	contact = Contact.objects.create(user_id='auth0|'+str(user.id),first_name=group.user.first_name,
    									last_name=group.user.last_name,job_title="Group account for "+str(group.radio_station.name),
    									organization=group.radio_station.name, country_id=group.radio_station.country.id,
    									phone_number='',role='group')

    partners = Knowledge_partner.objects.all()

    for partner in partners:
        if partner.user.email:
            email = partner.user.email
        else:
            email = partner.user.username+'@uliza_log.fm'

    	user = Auth0User.objects.create(username=partner.user.username, password=partner.user.password,
    									email=email, role='knowledge_partner',is_super_user=partner.user.is_superuser,
    									notify_on_log_create=True)

    	contact = Contact.objects.create(user_id=user.id,first_name=partner.user.first_name,
    									last_name=partner.user.last_name,job_title=partner.role,
    									organization=partner.organization, country_id=partner.country.id,
    									phone_number=partner.phone_number,role='knowledge_partner')

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0076_auto_20171031_1744'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='radio_station',
            field=models.IntegerField(default=None, null=True, blank=True),
        ),
        migrations.RunPython(change_accounts_to_contacts)
    ]
