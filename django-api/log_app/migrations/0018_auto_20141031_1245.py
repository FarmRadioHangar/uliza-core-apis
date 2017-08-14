# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0017_auto_20141031_1241'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='live_week_day',
            field=models.CharField(max_length=3, null=True, choices=[(b'MON', b'Monday'), (b'TUE', b'Tuesday'), (b'WED', b'Wednsday'), (b'THU', b'Thursday'), (b'FRI', b'Friday'), (b'SAT', b'Saturday'), (b'SUN', b'Sunday')]),
        ),
        migrations.AlterField(
            model_name='program',
            name='repeat_week_day',
            field=models.CharField(blank=True, max_length=3, null=True, choices=[(b'MON', b'Monday'), (b'TUE', b'Tuesday'), (b'WED', b'Wednsday'), (b'THU', b'Thursday'), (b'FRI', b'Friday'), (b'SAT', b'Saturday'), (b'SUN', b'Sunday')]),
        ),
    ]
