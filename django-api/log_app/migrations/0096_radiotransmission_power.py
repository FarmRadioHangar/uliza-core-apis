# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0095_auto_20180308_0656'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiotransmission',
            name='power',
            field=models.CharField(max_length=80, null=True, blank=True),
        ),
    ]
