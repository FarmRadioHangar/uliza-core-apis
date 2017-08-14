# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0063_log_case_study'),
    ]

    operations = [
        migrations.AlterField(
            model_name='country',
            name='language',
            field=models.CharField(max_length=6),
            preserve_default=True,
        ),
    ]
