# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Participants',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('phone_number', models.CharField(max_length=20)),
                ('registration_status', models.CharField(max_length=20, choices=[(b'NOT_REGISTERED', b'Not registered'), (b'REGISTERED', b'Registered'), (b'DECLINED', b'Declined')])),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
        ),
        migrations.CreateModel(
            name='RegistrationCall',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('phone_number', models.CharField(max_length=20)),
                ('scheduled_time', models.DateTimeField()),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
        ),
        migrations.AddField(
            model_name='participants',
            name='registration_call',
            field=models.ForeignKey(to='uliza.RegistrationCall'),
        ),
    ]
