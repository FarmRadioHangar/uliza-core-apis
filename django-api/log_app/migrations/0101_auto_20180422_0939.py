# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0100_format_legacy'),
    ]

    operations = [
        migrations.AddField(
            model_name='review',
            name='log',
            field=models.ForeignKey(default=1, to='log_app.Log'),
            preserve_default=False,
        ),
        migrations.AlterField(
            model_name='log',
            name='formats',
            field=models.ManyToManyField(to='log_app.Format', blank=True),
        ),
    ]
