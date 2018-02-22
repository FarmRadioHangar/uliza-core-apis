# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0020_auto_20180202_1704'),
    ]

    operations = [
        migrations.CreateModel(
            name='ParticipantRegistrationStatusEvent',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('event_type', models.CharField(max_length=100, choices=[(b'REGISTRATION_CALL_SCHEDULED', b'A registration call was scheduled'), (b'REGISTRATION_DECLINED', b'Registration declined'), (b'REGISTRATION_COMPLETE', b'Registration complete')])),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('participant', models.ForeignKey(to='uliza.Participant')),
            ],
            options={
                'db_table': 'uliza_participant_registration_status_events',
            },
        ),
        migrations.RemoveField(
            model_name='participantregistrationstatuslog',
            name='participant',
        ),
        migrations.RemoveField(
            model_name='participantregistrationstatuslog',
            name='registration_call',
        ),
        migrations.DeleteModel(
            name='VotoSurveyRegistrationTree',
        ),
        migrations.DeleteModel(
            name='VotoWebhookLog',
        ),
        migrations.RemoveField(
            model_name='registrationcall',
            name='interactions',
        ),
        migrations.RemoveField(
            model_name='registrationcall',
            name='phone_number',
        ),
        migrations.AlterField(
            model_name='registrationcall',
            name='voto_call_id',
            field=models.IntegerField(db_index=True),
        ),
        migrations.DeleteModel(
            name='ParticipantRegistrationStatusLog',
        ),
    ]
