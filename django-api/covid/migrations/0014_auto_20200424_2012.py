# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0013_chatuser_state'),
    ]

    operations = [
        migrations.AlterField(
            model_name='chatuser',
            name='state',
            field=models.IntegerField(default=-1, null=True, blank=True),
        ),
    ]
