# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0004_auto_20141024_1357'),
    ]

    operations = [
        migrations.AlterField(
            model_name='radiostation',
            name='lattitude',
            field=models.FloatField(null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='longitude',
            field=models.FloatField(null=True, blank=True),
        ),
    ]
