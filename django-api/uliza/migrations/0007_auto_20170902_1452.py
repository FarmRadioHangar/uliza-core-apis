# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0006_auto_20170901_0923'),
    ]

    operations = [
        migrations.CreateModel(
            name='VotoWebhookLog',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('endpoint', models.CharField(max_length=100)),
                ('data', models.TextField(null=True, blank=True)),
                ('log_time', models.DateTimeField(auto_now_add=True)),
            ],
            options={
                'db_table': 'uliza_voto_webhook_log',
            },
        ),
        migrations.DeleteModel(
            name='VotoResponseData',
        ),
    ]
