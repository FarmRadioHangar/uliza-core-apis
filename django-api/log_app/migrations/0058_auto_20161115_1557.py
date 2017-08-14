# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime
from django.utils.timezone import utc


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0057_auto_20161115_1411'),
    ]

    operations = [
        migrations.AddField(
            model_name='comment',
            name='created_at',
            field=models.DateTimeField(default=datetime.datetime(2016, 11, 15, 15, 57, 25, 263915, tzinfo=utc), auto_now_add=True),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='comment',
            name='last_updated_at',
            field=models.DateTimeField(default=datetime.datetime(2016, 11, 15, 15, 57, 36, 732455, tzinfo=utc), auto_now=True),
            preserve_default=False,
        ),
    ]
