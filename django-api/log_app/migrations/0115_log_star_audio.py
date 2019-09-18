# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0114_auto_20190719_0719'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='star_audio',
            field=models.BooleanField(default=False),
        ),
    ]
