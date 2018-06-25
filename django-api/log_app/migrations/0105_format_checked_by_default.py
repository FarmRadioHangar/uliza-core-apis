# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0104_auto_20180508_1341'),
    ]

    operations = [
        migrations.AddField(
            model_name='format',
            name='checked_by_default',
            field=models.BooleanField(default=False),
        ),
    ]
