# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0002_auto_20141024_1255'),
    ]

    operations = [
        migrations.AlterField(
            model_name='project',
            name='end_date',
            field=models.DateField(),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='email',
            field=models.EmailField(max_length=50, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='frequency',
            field=models.CharField(max_length=100, blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='lattitude',
            field=models.FloatField(null=True,blank=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='longitude',
            field=models.FloatField(null=True,blank=True),
        ),
    ]
