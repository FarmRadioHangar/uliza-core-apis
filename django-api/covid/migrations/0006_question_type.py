# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0005_auto_20200420_2006'),
    ]

    operations = [
        migrations.AddField(
            model_name='question',
            name='type',
            field=models.CharField(max_length=15, null=True, blank=True),
        ),
    ]
