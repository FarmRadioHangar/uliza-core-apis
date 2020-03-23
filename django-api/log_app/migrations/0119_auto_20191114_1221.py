# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0118_auto_20191101_1251'),
    ]

    operations = [
        migrations.AlterField(
            model_name='project',
            name='focus',
            field=models.TextField(),
        ),
    ]
