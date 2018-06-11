# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

log_users = {}

def nullify_user_register(apps,schema_editor):
    Log = apps.get_model('log_app', 'Log')
    logs = Log.objects.all()

    for l in logs:
        log_users[l.id] = l.saved_by
        l.saved_by = None
        l.save()

def replace_user_register(apps, schema_editor):
    Log = apps.get_model('log_app', 'Log')
    Contact = apps.get_model('log_app', 'Contact')

    for log_id in log_users:
        user_id = log_users[log_id]
        contact = Contact.objects.filter(user_id=user_id)
        id = None
        if contact:
            id = contact[0].id

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
