# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0099_auto_20180418_0740'),
    ]

    operations = [
        migrations.AddField(
            model_name='format',
            name='legacy',
            field=models.BooleanField(default=False),
        ),
    ]
