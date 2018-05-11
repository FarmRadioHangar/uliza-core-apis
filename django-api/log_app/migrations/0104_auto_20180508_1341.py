# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0103_auto_20180508_1321'),
    ]

    operations = [
        migrations.AddField(
            model_name='checklist',
            name='description_am',
            field=models.TextField(null=True, blank=True),
        ),
        migrations.AddField(
            model_name='checklist',
            name='description_fr',
            field=models.TextField(null=True, blank=True),
        ),
        migrations.AddField(
            model_name='checklist',
            name='description_pt',
            field=models.TextField(null=True, blank=True),
        ),
        migrations.AddField(
            model_name='format',
            name='name_am',
            field=models.CharField(max_length=60, null=True, blank=True),
        ),
        migrations.AddField(
            model_name='format',
            name='name_fr',
            field=models.CharField(max_length=60, null=True, blank=True),
        ),
        migrations.AddField(
            model_name='format',
            name='name_pt',
            field=models.CharField(max_length=60, null=True, blank=True),
        ),
    ]
