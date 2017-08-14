# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0060_auto_20161118_1329'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiostation',
            name='uliza_password',
            field=models.CharField(max_length=50, null=True, blank=True),
            preserve_default=True,
        ),
    ]
