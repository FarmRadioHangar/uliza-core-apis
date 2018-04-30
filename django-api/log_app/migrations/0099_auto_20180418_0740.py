# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0098_log_formats'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='review',
            name='log',
        ),
        migrations.RemoveField(
            model_name='review',
            name='user',
        ),
        migrations.AddField(
            model_name='review',
            name='reviewer',
            field=models.ForeignKey(default=1, to='log_app.Contact'),
            preserve_default=False,
        ),
    ]
