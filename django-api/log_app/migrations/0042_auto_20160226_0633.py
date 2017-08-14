# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0041_auto_20151018_0841'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='offset',
            field=models.PositiveIntegerField(default=0),
            preserve_default=True,
        ),
    ]
