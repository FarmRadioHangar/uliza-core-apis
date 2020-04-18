# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0001_initial'),
    ]

    operations = [
        migrations.RenameField(
            model_name='content',
            old_name='content',
            new_name='content_en',
        ),
        migrations.RenameField(
            model_name='content',
            old_name='topic',
            new_name='topic_en',
        ),
    ]
