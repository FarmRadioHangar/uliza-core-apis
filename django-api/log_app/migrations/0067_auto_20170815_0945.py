# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0066_auto_20170512_0658'),
    ]

    operations = [
        migrations.AlterField(
            model_name='log',
            name='recording',
            field=models.FileField(null=True, upload_to=b'/FRI-LOG', blank=True),
        ),
    ]
