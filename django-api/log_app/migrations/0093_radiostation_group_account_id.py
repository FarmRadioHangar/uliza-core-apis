# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def migrate_group_accounts(apps,schema_editor):
    Contact = apps.get_model('log_app', 'Contact')
    RadioStation = apps.get_model('log_app','RadioStation')
    Log = apps.get_model('log_app','Log')

    groups = Contact.objects.filter(role='group')

    for group in groups:
        radio_station = RadioStation.objects.filter(name=group.organization)[0]
        radio_station.group_account_id = group.user_id
        radio_station.save()
        logs = Log.objects.filter(saved_by=group)

        for log in logs:
            log.saved_by = None
            log.save()
            
        group.delete()

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0092_remove_comment_user'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiostation',
            name='group_account_id',
            field=models.CharField(max_length=120, null=True, blank=True),
        ),
        migrations.RunPython(migrate_group_accounts)
    ]
