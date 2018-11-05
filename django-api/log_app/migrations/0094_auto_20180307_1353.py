# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


def migrate_radio_transmission(apps,schema_editor):
    RadioTransmission = apps.get_model('log_app','RadioTransmission')
    RadioStation = apps.get_model('log_app','RadioStation')

    stations = RadioStation.objects.all()

    for station in stations:
        if station.frequency:
            RadioTransmission.objects.create(frequency=station.frequency,
                                             gain = station.transmission_gain,
                                             coordinates=str(station.longitude)+','+str(station.lattitude),
                                             height=station.tower_height,
                                             power=station.transmission_power,
                                             radio_station=station)


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0093_radiostation_group_account_id'),
    ]

    operations = [
        migrations.CreateModel(
            name='RadioTransmission',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('frequency', models.CharField(max_length=80, null=True, blank=True)),
                ('gain', models.CharField(max_length=80, null=True, blank=True)),
                ('height', models.CharField(max_length=80, null=True, blank=True)),
                ('coordinates', models.CharField(max_length=120, null=True, blank=True)),
                ('radio_station', models.ForeignKey(to='log_app.RadioStation')),
            ],
        ),
        migrations.AddField(
            model_name='radiotransmission',
            name='power',
            field=models.CharField(max_length=80, null=True, blank=True),
        ),
        migrations.RunPython(migrate_radio_transmission),
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
    ]
