# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0016_auto_20141031_1152'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='live_end_time',
            field=models.TimeField(null=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='live_start_time',
            field=models.TimeField(null=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='live_week_day',
            field=models.CharField(max_length=1, null=True, choices=[(b'MON', b'Monday'), (b'TUE', b'Tuesday'), (b'WED', b'Wednsday'), (b'THU', b'Thursday'), (b'FRI', b'Friday'), (b'SAT', b'Saturday'), (b'SUN', b'Sunday')]),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='repeat_end_time',
            field=models.TimeField(null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='repeat_start_time',
            field=models.TimeField(null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='repeat_week_day',
            field=models.CharField(blank=True, max_length=1, null=True, choices=[(b'MON', b'Monday'), (b'TUE', b'Tuesday'), (b'WED', b'Wednsday'), (b'THU', b'Thursday'), (b'FRI', b'Friday'), (b'SAT', b'Saturday'), (b'SUN', b'Sunday')]),
            preserve_default=True,
        ),
    ]
