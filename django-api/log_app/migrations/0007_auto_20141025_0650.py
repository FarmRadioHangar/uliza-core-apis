# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0006_auto_20141024_1406'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='end_date',
            field=models.DateField(null=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='start_date',
            field=models.DateField(null=True),
            preserve_default=True,
        ),
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
