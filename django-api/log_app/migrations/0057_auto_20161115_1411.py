# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0056_comment'),
    ]

    operations = [
        migrations.RenameField(
            model_name='comment',
            old_name='log_id',
            new_name='log',
        ),
    ]
