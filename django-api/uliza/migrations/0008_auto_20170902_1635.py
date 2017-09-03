# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0007_auto_20170902_1452'),
    ]

    operations = [
        migrations.RenameField(
            model_name='votowebhooklog',
            old_name='endpoint',
            new_name='webhook',
        ),
    ]
