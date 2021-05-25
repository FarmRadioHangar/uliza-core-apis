# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0130_project_reference_links'),
    ]

    operations = [
        migrations.AlterField(
            model_name='poddistributionlog',
            name='triggered_by',
            field=models.ForeignKey(to='log_app.Contact', null=True),
        ),
    ]
