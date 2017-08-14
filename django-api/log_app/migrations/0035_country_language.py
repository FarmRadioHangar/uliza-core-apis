# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0034_auto_20150709_0832'),
    ]

    operations = [
        migrations.AddField(
            model_name='country',
            name='language',
            field=models.CharField(default='en', max_length=3),
            preserve_default=False,
        ),
    ]
