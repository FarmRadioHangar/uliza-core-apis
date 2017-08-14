# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0040_auto_20151018_0746'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='end_date',
            field=models.DateField(null=True, blank=True),
            preserve_default=True,
        ),
    ]
