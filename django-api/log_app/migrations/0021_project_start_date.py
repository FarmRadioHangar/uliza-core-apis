# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0020_log_poll'),
    ]

    operations = [
        migrations.AddField(
            model_name='project',
            name='start_date',
            field=models.DateField(null=True),
            preserve_default=True,
        ),
    ]
