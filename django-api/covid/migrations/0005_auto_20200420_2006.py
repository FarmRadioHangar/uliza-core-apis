# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0004_auto_20200420_1227'),
    ]

    operations = [
        migrations.AlterField(
            model_name='chatuser',
            name='country',
            field=models.CharField(max_length=10, null=True, blank=True),
        ),
    ]
