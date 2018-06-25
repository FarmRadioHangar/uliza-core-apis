# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0097_auto_20180416_0802'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='formats',
            field=models.ManyToManyField(to='log_app.Format', null=True, blank=True),
        ),
    ]
