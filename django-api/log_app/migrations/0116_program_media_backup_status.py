# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0115_log_star_audio'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='media_backup_status',
            field=models.CharField(max_length=150, blank=True),
        ),
    ]
