# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0003_auto_20200420_1218'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='chatuser',
            name='chat_id',
        ),
        migrations.AddField(
            model_name='chatuser',
            name='user_id',
            field=models.CharField(default=9999, unique=True, max_length=100),
            preserve_default=False,
        ),
    ]
