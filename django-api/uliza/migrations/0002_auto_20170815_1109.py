# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='ParticipantRegistrationStatusLog',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('event_type', models.CharField(max_length=20, choices=[(b'REGISTRATION_CALL_SCHEDULED', b'A registration call was scheduled'), (b'REGISTRATION_DECLINED', b'Registration declined'), (b'REGISTRATION_COMPLETE', b'Registration complete')])),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
        ),
        migrations.CreateModel(
            name='VotoResponseData',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('data', models.TextField(null=True, blank=True)),
            ],
        ),
        migrations.AlterField(
            model_name='participants',
            name='registration_call',
            field=models.ForeignKey(to='uliza.RegistrationCall', null=True),
        ),
        migrations.AddField(
            model_name='participantregistrationstatuslog',
            name='participant',
            field=models.ForeignKey(to='uliza.Participants'),
        ),
        migrations.AddField(
            model_name='participantregistrationstatuslog',
            name='registration_call',
            field=models.ForeignKey(to='uliza.RegistrationCall', null=True),
        ),
    ]
