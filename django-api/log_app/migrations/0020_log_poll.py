# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0019_log_week'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='poll',
            field=models.TextField(null=True, blank=True),
            preserve_default=True,
        ),
    ]
