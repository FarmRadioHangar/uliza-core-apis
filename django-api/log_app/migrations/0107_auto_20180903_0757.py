# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0106_auto_20180625_1049'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='created_at',
            field=models.DateTimeField(default=datetime.datetime(2018, 9, 3, 7, 57, 14, 144684), auto_now_add=True),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='contact',
            name='last_updated_at',
            field=models.DateTimeField(default=datetime.datetime(2018, 9, 3, 7, 57, 24, 924541), auto_now=True),
            preserve_default=False,
        ),
    ]
