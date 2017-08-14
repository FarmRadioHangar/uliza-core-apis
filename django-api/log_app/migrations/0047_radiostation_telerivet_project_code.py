# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0046_program_uliza'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiostation',
            name='telerivet_project_code',
            field=models.CharField(max_length=50, null=True, blank=True),
            preserve_default=True,
        ),
    ]
