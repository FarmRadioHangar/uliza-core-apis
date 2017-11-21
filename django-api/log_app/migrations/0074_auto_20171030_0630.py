# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import django.utils.timezone


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0073_auto_20171030_0627'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(default=django.utils.timezone.now),
        ),
    ]
