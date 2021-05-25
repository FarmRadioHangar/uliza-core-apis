# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0125_review_void_formats'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiostation',
            name='premium_account',
            field=models.BooleanField(default=False),
        ),
    ]
