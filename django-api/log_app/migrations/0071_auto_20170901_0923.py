# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0070_auto_20170824_1035'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(),
        ),
    ]
