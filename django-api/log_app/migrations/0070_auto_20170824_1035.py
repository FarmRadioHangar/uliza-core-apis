# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0069_auto_20170823_0728'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='duration',
            field=models.IntegerField(default=30),
        ),
        migrations.AlterField(
            model_name='program',
            name='end_date',
            field=models.DateField(null=True),
        ),
        migrations.AlterField(
            model_name='program',
            name='weeks',
            field=models.IntegerField(),
        ),
    ]
