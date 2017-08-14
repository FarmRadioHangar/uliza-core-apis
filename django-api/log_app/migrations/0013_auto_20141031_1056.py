# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0012_auto_20141031_0828'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='program_presenters',
            name='presenter',
        ),
        migrations.RemoveField(
            model_name='program_presenters',
            name='program',
        ),
        migrations.RemoveField(
            model_name='program',
            name='journalists',
        ),
        migrations.DeleteModel(
            name='Program_presenters',
        ),
        migrations.AddField(
            model_name='presenter',
            name='role',
            field=models.CharField(max_length=64, null=True),
            preserve_default=True,
        ),
    ]
