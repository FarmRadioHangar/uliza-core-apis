# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0044_knowledge_partner'),
    ]

    operations = [
        migrations.AddField(
            model_name='knowledge_partner',
            name='role',
            field=models.CharField(max_length=64, null=True),
            preserve_default=True,
        ),
    ]
