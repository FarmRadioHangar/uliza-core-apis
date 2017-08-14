# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0009_auto_20141031_0656'),
    ]

    operations = [
        migrations.AlterField(
            model_name='radiostation',
            name='manager',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='tower_height',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='tower_location',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='transmission_gain',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='transmission_power',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='website',
            field=models.CharField(max_length=50,null=True, blank=True),
        ),
    ]
