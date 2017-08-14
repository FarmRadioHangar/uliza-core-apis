# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0025_auto_20141111_0442'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='duration',
            field=models.IntegerField(null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='log',
            name='focus_statement',
            field=models.TextField(null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='log',
            name='topic',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='log',
            name='week',
            field=models.IntegerField(null=True, blank=True),
        ),
    ]
