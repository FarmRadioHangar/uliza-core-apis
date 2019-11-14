# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models
import log_app.storage.gd_storage


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0117_auto_20190925_0914'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='gdrive',
            field=models.FileField(storage=log_app.storage.gd_storage.GoogleDriveStorage(), null=True, upload_to=b'/FRI-LOG', blank=True),
        ),
        migrations.AlterField(
            model_name='log',
            name='topic',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
    ]
