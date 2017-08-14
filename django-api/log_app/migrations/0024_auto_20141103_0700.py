# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0023_program_duration'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='program',
            name='live_end_time',
        ),
        migrations.RemoveField(
            model_name='program',
            name='repeat_end_time',
        ),
        migrations.AlterField(
            model_name='program',
            name='repeat_week_day',
            field=models.CharField(blank=True, max_length=3, null=True, choices=[(b'Mon', b'Monday'), (b'Tue', b'Tuesday'), (b'Wed', b'Wednsday'), (b'Thu', b'Thursday'), (b'Fri', b'Friday'), (b'Sat', b'Saturday'), (b'Sun', b'Sunday')]),
        ),
    ]
