# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0068_auto_20170818_1003'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='program',
            name='journalists',
        ),
        migrations.RemoveField(
            model_name='program',
            name='knowledge_partner',
        ),
    ]
