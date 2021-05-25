# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0129_auto_20210317_1242'),
    ]

    operations = [
        migrations.AddField(
            model_name='project',
            name='reference_links',
            field=models.TextField(null=True, blank=True),
        ),
    ]
