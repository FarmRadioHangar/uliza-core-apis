# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0012_auto_20200424_1204'),
    ]

    operations = [
        migrations.AddField(
            model_name='chatuser',
            name='state',
            field=models.IntegerField(default=0, null=True, blank=True),
        ),
    ]
