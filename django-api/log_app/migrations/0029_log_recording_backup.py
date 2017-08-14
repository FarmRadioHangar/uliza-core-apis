# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration): 

    dependencies = [
        ('log_app', '0028_auto_20150128_1241'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='recording_backup',
            field=models.FileField(null=True, upload_to=b'/home/jix/fri-log/fri-log/media/', blank=True),
        ),
    ]
