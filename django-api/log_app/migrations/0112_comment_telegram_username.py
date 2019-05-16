# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0111_log_gdrive_url'),
    ]

    operations = [
        migrations.AddField(
            model_name='comment',
            name='telegram_username',
            field=models.CharField(max_length=80, null=True),
        ),
    ]
