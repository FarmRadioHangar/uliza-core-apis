# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0086_auto_20180205_1311'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='notify_on_log_create',
            field=models.BooleanField(default=False),
        ),
    ]
