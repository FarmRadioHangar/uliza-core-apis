# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0010_auto_20141031_0712'),
    ]

    operations = [
        migrations.AlterField(
            model_name='radiostation',
            name='email',
            field=models.EmailField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='frequency',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='manager',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='tower_height',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='tower_location',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='transmission_gain',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='transmission_power',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='website',
            field=models.CharField(max_length=50, null=True, blank=True),
        ),
    ]
