# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0080_delete_account'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='access',
            field=models.ManyToManyField(to='log_app.Contact', blank=True),
        ),
    ]
