# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0027_auto_20141112_1132'),
    ]

    operations = [
        migrations.RenameField(
            model_name='log',
            old_name='poll',
            new_name='ict',
        ),
    ]
