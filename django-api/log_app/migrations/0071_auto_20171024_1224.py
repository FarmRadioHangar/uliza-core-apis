# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import datetime


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0070_auto_20170824_1035'),
    ]

    operations = [
        migrations.CreateModel(
            name='Contact',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('user_id', models.IntegerField()),
                ('first_name', models.CharField(max_length=30, null=True, blank=True)),
                ('last_name', models.CharField(max_length=30, null=True, blank=True)),
                ('job_title', models.CharField(max_length=100, null=True, blank=True)),
                ('phone_number', models.CharField(max_length=50, null=True, blank=True)),
                ('organization', models.CharField(max_length=64, null=True)),
                ('role', models.CharField(max_length=64, null=True)),
                ('language', models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'Portuguese'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')])),
                ('country', models.ForeignKey(to='log_app.Country', null=True)),
            ],
        ),
        migrations.AlterField(
            model_name='program',
            name='start_date',
            field=models.DateTimeField(default=datetime.datetime(2017, 10, 24, 12, 24, 32, 245914)),
        ),
    ]
