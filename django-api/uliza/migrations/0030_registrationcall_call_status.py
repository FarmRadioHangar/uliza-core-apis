# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0029_auto_20180215_1237'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='call_status',
            field=models.CharField(default='SCHEDULED', max_length=100, choices=[(b'SCHEDULED', b'Scheduled'), (b'COMPLETE', b'Complete')]),
            preserve_default=False,
        ),
    ]
