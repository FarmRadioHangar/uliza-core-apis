# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0023_registrationcall_interactions'),
    ]

    operations = [
        migrations.AlterField(
            model_name='registrationcall',
            name='voto_call_id',
            field=models.IntegerField(unique=True, db_index=True),
        ),
    ]
