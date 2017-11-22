# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0013_auto_20171115_0715'),
    ]

    operations = [
        migrations.AddField(
            model_name='participant',
            name='location',
            field=models.CharField(max_length=20, null=True),
        ),
    ]
