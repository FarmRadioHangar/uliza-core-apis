# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0009_auto_20170910_0913'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='voto_id',
            field=models.IntegerField(null=True),
        ),
    ]
