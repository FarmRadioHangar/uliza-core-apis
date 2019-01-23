# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0110_auto_20190115_0757'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='gdrive_url',
            field=models.URLField(max_length=400, null=True, blank=True),
        ),
    ]
