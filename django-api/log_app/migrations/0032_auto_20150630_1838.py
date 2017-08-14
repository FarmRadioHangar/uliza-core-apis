# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import log_app.models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0031_auto_20150620_1423'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='presenter',
            field=models.ForeignKey(blank=True, to='log_app.Presenter', null=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='recording_backup',
            field=models.FileField(null=True, upload_to=log_app.models.filename, blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='program',
            name='duration',
            field=models.IntegerField(default=30, null=True),
            preserve_default=True,
        ),
    ]
