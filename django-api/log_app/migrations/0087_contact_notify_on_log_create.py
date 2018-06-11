# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def add_notification_data(apps, schema_editor):
    Auth0User = apps.get_model('log_app','Auth0User')
    Contact = apps.get_model('log_app','Contact')
    users = Auth0User.objects.all()

    for user in users:
        c = Contact.objects.filter(user_id=user.id)

        if c:
            c = c[0]
            c.notify_on_log_create = user.notify_on_log_create
            c.save()

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0086_auto_20180205_1311'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='notify_on_log_create',
            field=models.BooleanField(default=False),
        ),
        migrations.RunPython(add_notification_data),
    ]
