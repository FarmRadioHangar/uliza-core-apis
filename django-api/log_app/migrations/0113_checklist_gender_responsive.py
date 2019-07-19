# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0112_comment_telegram_username'),
    ]

    operations = [
        migrations.AddField(
            model_name='checklist',
            name='gender_responsive',
            field=models.BooleanField(default=False),
        ),
    ]
