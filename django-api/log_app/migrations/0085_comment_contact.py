# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0084_contact_email'),
    ]

    operations = [
        migrations.AddField(
            model_name='comment',
            name='contact',
            field=models.ForeignKey(to='log_app.Contact', null=True),
        ),
    ]
