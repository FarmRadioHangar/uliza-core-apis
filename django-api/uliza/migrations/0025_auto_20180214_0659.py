# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0024_auto_20180214_0654'),
    ]

    operations = [
        migrations.AlterField(
            model_name='registrationcall',
            name='voto_call_id',
            field=models.IntegerField(unique=True),
        ),
    ]
