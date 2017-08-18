# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0003_auto_20170816_0646'),
    ]

    operations = [
        migrations.AlterField(
            model_name='participantregistrationstatuslog',
            name='event_type',
            field=models.CharField(max_length=100, choices=[(b'REGISTRATION_CALL_SCHEDULED', b'A registration call was scheduled'), (b'REGISTRATION_DECLINED', b'Registration declined'), (b'REGISTRATION_COMPLETE', b'Registration complete')]),
        ),
    ]
