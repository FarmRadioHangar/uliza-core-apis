# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0010_auto_20200420_2346'),
    ]

    operations = [
        migrations.AddField(
            model_name='content',
            name='title',
            field=models.CharField(default='none', max_length=100),
            preserve_default=False,
        ),
    ]
