# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0062_auto_20161217_0942'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='case_study',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
    ]
