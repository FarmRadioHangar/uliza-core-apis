# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

log_users = {}

def nullify_user_register(apps,schema_editor):
    Log = apps.get_model('log_app', 'Log')
    logs = Log.objects.all()

    for l in logs:
        if l.saved_by:
            log_users[l.id] = l.saved_by.username

        l.saved_by = None
        l.save()

def replace_user_register(apps, schema_editor):
    Log = apps.get_model('log_app', 'Log')
    Contact = apps.get_model('log_app', 'Contact')
    Auth0User = apps.get_model('log_app', 'Auth0User')

    for log_id in log_users:
        username = log_users[log_id]
        user = Auth0User.objects.filter(username=username)
        id = None

        if user:
            contact = Contact.objects.filter(user_id='auth0|'+str(user[0].id))
            if contact:
                id = contact[0]

            log = Log.objects.get(pk=log_id)
            log.saved_by = id
            log.save()


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0090_remove_log_presenter'),
    ]

    operations = [
        migrations.RunPython(nullify_user_register),
        migrations.AlterField(
            model_name='log',
            name='saved_by',
            field=models.ForeignKey(blank=True, to='log_app.Contact', null=True),
        ),
        migrations.RunPython(replace_user_register),
    ]
