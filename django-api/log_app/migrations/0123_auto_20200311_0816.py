# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0122_comment_training_call'),
    ]

    operations = [
        migrations.CreateModel(
            name='BroadcasterResource',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=100)),
                ('name_fr', models.CharField(max_length=100)),
                ('name_pt', models.CharField(max_length=100)),
                ('name_am', models.CharField(max_length=100)),
                ('description', models.TextField(null=True, blank=True)),
            ],
        ),
        migrations.AddField(
            model_name='log',
            name='link_to_resource',
            field=models.URLField(max_length=400, null=True, blank=True),
        ),
        migrations.AddField(
            model_name='log',
            name='broadcaster_resource',
            field=models.ForeignKey(blank=True, to='log_app.BroadcasterResource', null=True),
        ),
    ]
