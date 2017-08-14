# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0015_auto_20141031_1105'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='presenter',
            name='email',
        ),
        migrations.AddField(
            model_name='program',
            name='weeks',
            field=models.IntegerField(default=10),
            preserve_default=True,
        ),
    ]
