# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0004_auto_20170818_1003'),
    ]

    operations = [
        migrations.AlterField(
            model_name='participant',
            name='registration_status',
            field=models.CharField(max_length=100, choices=[(b'NOT_REGISTERED', b'Not registered'), (b'REGISTERED', b'Registered'), (b'DECLINED', b'Declined')]),
        ),
    ]
