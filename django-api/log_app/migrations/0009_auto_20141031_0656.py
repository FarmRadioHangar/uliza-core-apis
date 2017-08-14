# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0008_auto_20141031_0647'),
    ]

    operations = [
        migrations.AlterField(
            model_name='radiostation',
            name='phone_number',
            field=models.CharField(max_length=50, null=True,blank=True),
        ),
    ]
