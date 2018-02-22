# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime
from django.utils.timezone import utc


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0026_auto_20180215_1052'),
    ]

    operations = [
        migrations.AlterField(
            model_name='registrationcall',
            name='schedule_time',
            field=models.DateTimeField(default=datetime.datetime(2018, 2, 15, 11, 24, 36, 225731, tzinfo=utc)),
            preserve_default=False,
        ),
        migrations.AlterField(
            model_name='registrationcall',
            name='voto_call_id',
            field=models.IntegerField(unique=True, null=True),
        ),
    ]
