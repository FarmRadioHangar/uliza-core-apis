# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0036_account'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='notified_via_email',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='project',
            name='country',
            field=models.ForeignKey(to='log_app.Country'),
            preserve_default=False,
        ),
    ]
