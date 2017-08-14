# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0047_radiostation_telerivet_project_code'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='uliza',
            field=models.CharField(max_length=50, null=True, blank=True),
            preserve_default=True,
        ),
    ]
