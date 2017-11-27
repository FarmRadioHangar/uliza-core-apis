# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0079_auto_20171121_1429'),
    ]

    operations = [
        migrations.DeleteModel(
            name='Account',
        ),
    ]
