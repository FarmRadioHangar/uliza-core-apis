# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0009_auto_20200420_2327'),
    ]

    operations = [
        migrations.AlterField(
            model_name='question',
            name='chat_user',
            field=models.ForeignKey(blank=True, to='covid.ChatUser', null=True),
        ),
    ]
