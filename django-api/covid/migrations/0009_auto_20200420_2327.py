# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0008_auto_20200420_2315'),
    ]

    operations = [
        migrations.RenameField(
            model_name='question',
            old_name='chat_id',
            new_name='chat_user',
        ),
    ]
