# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import log_app.storage.gd_storage


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0026_auto_20141112_0748'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='recording',
            field=models.FileField(storage=None, null=True, upload_to=b'/programs', blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='topic',
            field=models.CharField(max_length=30, null=True, blank=True),
        ),
    ]
