# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0037_auto_20150711_1951'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='confirmed_program_time',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
    ]
