# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0072_auto_20171024_1341'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(default=datetime.datetime(2017, 10, 30, 6, 27, 21, 683603)),
        ),
    ]
