# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0059_merge'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='question_answer',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
    ]
