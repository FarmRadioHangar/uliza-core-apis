# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0024_auto_20141103_0700'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='postpone',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='log',
            name='postponed_for',
            field=models.TextField(null=True, blank=True),
            preserve_default=True,
        ),
    ]
