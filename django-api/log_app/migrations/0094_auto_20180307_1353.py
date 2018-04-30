# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0093_radiostation_group_account_id'),
    ]

    operations = [
        migrations.CreateModel(
            name='TransmissionTower',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('frequency', models.CharField(max_length=80, null=True, blank=True)),
                ('gain', models.CharField(max_length=80, null=True, blank=True)),
                ('height', models.CharField(max_length=80, null=True, blank=True)),
                ('coordinates', models.CharField(max_length=120, null=True, blank=True)),
            ],
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='frequency',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='lattitude',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='longitude',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='telerivet_project_code',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='tower_height',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='tower_location',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='transmission_gain',
        ),
        migrations.RemoveField(
            model_name='radiostation',
            name='transmission_power',
        ),
        migrations.AddField(
            model_name='transmissiontower',
            name='radio_station',
            field=models.ForeignKey(to='log_app.RadioStation'),
        ),
    ]
