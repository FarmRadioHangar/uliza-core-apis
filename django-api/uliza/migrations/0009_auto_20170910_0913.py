# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0008_auto_20170902_1635'),
    ]

    operations = [
        migrations.RenameField(
            model_name='votowebhooklog',
            old_name='webhook',
            new_name='endpoint',
        ),
    ]
