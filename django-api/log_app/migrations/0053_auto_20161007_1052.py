# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0052_log_user'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='knowledge_partner',
            name='project',
        ),
        migrations.AddField(
            model_name='program',
            name='knowledge_partner',
            field=models.ManyToManyField(to='log_app.Knowledge_partner', null=True, blank=True),
            preserve_default=True,
        ),
    ]
