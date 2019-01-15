# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0109_auto_20190115_0705'),
    ]

    operations = [
        migrations.RenameField(
            model_name='log',
            old_name='recording',
            new_name='gdrive',
        ),
        migrations.AddField(
            model_name='log',
            name='gdrive_available',
            field=models.BooleanField(default=False),
        ),
    ]
