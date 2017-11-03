# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0076_auto_20171031_1744'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='radio_station',
            field=models.IntegerField(default=None, null=True, blank=True),
        ),
    ]
