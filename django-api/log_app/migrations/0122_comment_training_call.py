# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0121_project_code'),
    ]

    operations = [
        migrations.AddField(
            model_name='comment',
            name='training_call',
            field=models.BooleanField(default=False),
        ),
    ]
