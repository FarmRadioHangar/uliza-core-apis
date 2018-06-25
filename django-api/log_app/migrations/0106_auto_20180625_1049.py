# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0105_format_checked_by_default'),
    ]

    operations = [
        migrations.RenameField(
            model_name='format',
            old_name='checked_by_default',
            new_name='always_checked',
        ),
    ]
