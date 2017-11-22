# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0014_participant_location'),
    ]

    operations = [
        migrations.AlterField(
            model_name='participant',
            name='location',
            field=models.CharField(default=b'NULL', max_length=20, null=True),
        ),
    ]
