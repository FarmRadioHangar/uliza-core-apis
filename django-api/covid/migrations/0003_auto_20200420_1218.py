# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0002_auto_20200417_1547'),
    ]

    operations = [
        migrations.AddField(
            model_name='chatuser',
            name='full_name',
            field=models.CharField(max_length=25, null=True, blank=True),
        ),
        migrations.AddField(
            model_name='chatuser',
            name='radio_station',
            field=models.CharField(max_length=25, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='chatuser',
            name='language',
            field=models.CharField(max_length=6, null=True, blank=True),
        ),
    ]
