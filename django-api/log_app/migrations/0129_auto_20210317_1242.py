# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0128_auto_20210202_1214'),
    ]

    operations = [
        migrations.RenameField(
            model_name='poddistributionlog',
            old_name='description',
            new_name='note',
        ),
        migrations.AlterField(
            model_name='program',
            name='uliza',
            field=models.CharField(max_length=80, null=True, blank=True),
        ),
    ]
