# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0061_radiostation_uliza_password'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='log',
            name='notified_via_email',
        ),
        migrations.AddField(
            model_name='log',
            name='email',
            field=models.TextField(default=None, null=True, blank=True),
            preserve_default=True,
        ),
    ]
