# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0107_auto_20180903_0757'),
    ]

    operations = [
        migrations.AddField(
            model_name='project',
            name='image',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
    ]
