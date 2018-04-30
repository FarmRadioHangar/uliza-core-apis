# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0101_auto_20180422_0939'),
    ]

    operations = [
        migrations.AddField(
            model_name='country',
            name='gbb',
            field=models.BooleanField(default=False),
        ),
    ]
