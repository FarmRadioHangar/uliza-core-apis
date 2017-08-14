# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0042_auto_20160226_0633'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='recording_saved',
            field=models.BooleanField(default=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='recording_backup',
            field=models.FileField(null=True, upload_to=b'', blank=True),
            preserve_default=True,
        ),
    ]
