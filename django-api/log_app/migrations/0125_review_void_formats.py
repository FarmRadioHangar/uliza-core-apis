# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0124_auto_20200323_0944'),
    ]

    operations = [
        migrations.AddField(
            model_name='review',
            name='void_formats',
            field=models.ManyToManyField(to='log_app.Format', blank=True),
        ),
    ]
