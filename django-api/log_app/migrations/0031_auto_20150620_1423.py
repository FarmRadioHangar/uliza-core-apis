# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0030_auto_20150603_1205'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='journalists',
            field=models.ManyToManyField(to='log_app.Presenter', null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='program',
            name='weeks',
            field=models.IntegerField(default=16),
            preserve_default=True,
        ),
    ]
