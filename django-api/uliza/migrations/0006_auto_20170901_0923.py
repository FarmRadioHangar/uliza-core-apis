# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0005_auto_20170821_1441'),
    ]

    operations = [
        migrations.AlterModelTable(
            name='participant',
            table='uliza_participants',
        ),
        migrations.AlterModelTable(
            name='participantregistrationstatuslog',
            table='uliza_participant_registration_status_log',
        ),
        migrations.AlterModelTable(
            name='registrationcall',
            table='uliza_registration_calls',
        ),
        migrations.AlterModelTable(
            name='votoresponsedata',
            table='uliza_voto_response_data',
        ),
    ]
