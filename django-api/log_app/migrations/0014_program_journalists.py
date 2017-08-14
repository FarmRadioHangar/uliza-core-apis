# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0013_auto_20141031_1056'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='journalists',
            field=models.ManyToManyField(to='log_app.Presenter'),
            preserve_default=True,
        ),
    ]
