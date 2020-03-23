# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models
import datetime


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0119_auto_20191114_1221'),
    ]

    operations = [
        migrations.AddField(
            model_name='checklist',
            name='created_at',
            field=models.DateTimeField(default=datetime.datetime(2019, 11, 27, 8, 40, 51, 94880), auto_now_add=True),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='checklist',
            name='last_updated_at',
            field=models.DateTimeField(default=datetime.datetime(2019, 11, 27, 8, 41, 0, 371278), auto_now=True),
            preserve_default=False,
        ),
    ]
