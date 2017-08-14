# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0021_project_start_date'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='program',
            name='end_date',
        ),
        migrations.RemoveField(
            model_name='program',
            name='live_start_time',
        ),
        migrations.RemoveField(
            model_name='program',
            name='live_week_day',
        ),
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(null=True),
        ),
    ]
