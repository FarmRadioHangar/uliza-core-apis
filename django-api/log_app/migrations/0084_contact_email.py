# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def add_email_data(apps, schema_editor):
    Auth0User = apps.get_model('log_app','Auth0User')
    Contact = apps.get_model('log_app','Contact')
    users = Auth0User.objects.all()

    for user in users:
        c = Contact.objects.filter(user_id='auth0|'+str(user.id))

        if c:
            c = c[0]
            c.email = user.email
            c.save()

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0083_auto_20171214_1751'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='email',
            field=models.EmailField(max_length=50, null=True, blank=True),
        ),
        migrations.RunPython(add_email_data),
    ]
