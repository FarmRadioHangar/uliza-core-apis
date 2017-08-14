# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0053_auto_20161007_1052'),
    ]

    operations = [
        migrations.AddField(
            model_name='knowledge_partner',
            name='country',
            field=models.ForeignKey(to='log_app.Country', null=True),
            preserve_default=True,
        ),
    ]
