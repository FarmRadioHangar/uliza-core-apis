# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0113_checklist_gender_responsive'),
    ]

    operations = [
        migrations.AlterField(
            model_name='checklist',
            name='level',
            field=models.CharField(default=b'good', max_length=6, choices=[(b'best', b'Best'), (b'good', b'Good'), (b'better', b'Better')]),
        ),
    ]
