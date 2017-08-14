# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0038_program_confirmed_program_time'),
    ]

    operations = [
        migrations.AddField(
            model_name='administrator',
            name='notify_daily_schedule',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='administrator',
            name='notify_log_submission',
            field=models.BooleanField(default=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='administrator',
            name='notify_signup',
            field=models.BooleanField(default=True),
            preserve_default=True,
        ),
    ]
