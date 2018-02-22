# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0022_auto_20180207_1337'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='interactions',
            field=models.TextField(null=True),
        ),
    ]
