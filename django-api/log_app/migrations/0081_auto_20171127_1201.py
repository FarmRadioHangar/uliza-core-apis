# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

program_users = {}

def nullify_user_access(apps,schema_editor):
    Program = apps.get_model('log_app', 'Program')
    programs = Program.objects.exclude(access=None)

    for p in programs:
        program_users[p.id] = [u.username for u in p.access.all()]
        p.access = []
        p.save()

def replace_contact_access(apps, schema_editor):
    Program = apps.get_model('log_app', 'Program')
    Auth0User = apps.get_model('log_app', 'Auth0User')
    Contact = apps.get_model('log_app', 'Contact')

    for program_id in program_users:
        access = []

        if program_users[program_id]:
            program = Program.objects.get(pk=program_id)

            for username in program_users[program_id]:
                user = Auth0User.objects.filter(username=username)

                if user:
                    contact = Contact.objects.filter(user_id='local|'+str(user[0].id))
                    access.append(contact[0].id)

            program.access = access
            program.save()

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0080_delete_account'),
    ]

    # take the user_ids in all the access ManyToManyField
    operations = [

        migrations.RunPython(nullify_user_access),
        migrations.AlterField(
            model_name='program',
            name='access',
            field=models.ManyToManyField(to='log_app.Contact', blank=True),
        ),
        migrations.RunPython(replace_contact_access),
    ]

    # after the field is altered add the contact_id in the new field
