# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0120_auto_20191127_0841'),
    ]

    operations = [
        migrations.AddField(
            model_name='project',
            name='code',
            field=models.CharField(default='None', max_length=100),
            preserve_default=False,
        ),
    ]
