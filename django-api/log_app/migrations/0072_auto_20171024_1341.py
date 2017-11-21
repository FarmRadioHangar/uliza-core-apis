# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0071_auto_20171024_1224'),
    ]

    operations = [
        migrations.AlterField(
            model_name='contact',
            name='user_id',
            field=models.CharField(max_length=100, null=True),
        ),
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(default=datetime.datetime(2017, 10, 24, 13, 41, 39, 509022)),
        ),
    ]
