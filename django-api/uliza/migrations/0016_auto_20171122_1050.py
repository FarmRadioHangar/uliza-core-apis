# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0015_auto_20171122_0907'),
    ]

    operations = [
        migrations.AlterField(
            model_name='participant',
            name='location',
            field=models.CharField(max_length=100, null=True),
        ),
    ]
