# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0090_remove_log_presenter'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='saved_by',
            field=models.ForeignKey(blank=True, to='log_app.Contact', null=True),
        ),
    ]
