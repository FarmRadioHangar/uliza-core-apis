# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0094_auto_20180307_1353'),
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
        migrations.RemoveField(
            model_name='transmissiontower',
            name='radio_station',
        ),
        migrations.DeleteModel(
            name='TransmissionTower',
        ),
    ]
