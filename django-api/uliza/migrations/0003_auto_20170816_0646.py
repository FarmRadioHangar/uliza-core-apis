# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0002_auto_20170815_1109'),
    ]

    operations = [
        migrations.CreateModel(
            name='Participant',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('phone_number', models.CharField(max_length=20)),
                ('registration_status', models.CharField(max_length=20, choices=[(b'NOT_REGISTERED', b'Not registered'), (b'REGISTERED', b'Registered'), (b'DECLINED', b'Declined')])),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
            options={
                'db_table': 'participants',
            },
        ),
        migrations.RemoveField(
            model_name='participants',
            name='registration_call',
        ),
        migrations.AlterField(
            model_name='participantregistrationstatuslog',
            name='participant',
            field=models.ForeignKey(to='uliza.Participant'),
        ),
        migrations.AlterModelTable(
            name='participantregistrationstatuslog',
            table='participant_registration_status_log',
        ),
        migrations.AlterModelTable(
            name='registrationcall',
            table='registration_calls',
        ),
        migrations.AlterModelTable(
            name='votoresponsedata',
            table='voto_response_data',
        ),
        migrations.DeleteModel(
            name='Participants',
        ),
        migrations.AddField(
            model_name='participant',
            name='registration_call',
            field=models.ForeignKey(to='uliza.RegistrationCall', null=True),
        ),
    ]
