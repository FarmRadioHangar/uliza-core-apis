# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0039_auto_20150804_1115'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='end_date',
            field=models.DateTimeField(null=True, blank=True),
            preserve_default=True,
        ),
    ]
