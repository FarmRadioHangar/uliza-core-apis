# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0127_auto_20201202_1155'),
    ]

    operations = [
        migrations.CreateModel(
            name='Notification',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('heading', models.CharField(max_length=80, null=True, blank=True)),
                ('content', models.TextField(default=b'None', null=True, blank=True)),
                ('seen', models.BooleanField(default=False)),
                ('url_model', models.CharField(max_length=80, null=True, blank=True)),
                ('link', models.IntegerField(null=True, blank=True)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('sent_to', models.ForeignKey(to='log_app.Contact')),
            ],
        ),
        migrations.CreateModel(
            name='PodDistributionLog',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('description', models.TextField(default=b'None', null=True, blank=True)),
                ('apple_podcasts_status', models.CharField(default=None, max_length=15, null=True, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')])),
                ('spotify_status', models.CharField(default=None, max_length=15, null=True, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')])),
                ('google_podcasts_status', models.CharField(default=None, max_length=15, null=True, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')])),
                ('podcast_addict_status', models.CharField(default=None, max_length=15, null=True, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')])),
                ('amazon_music_status', models.CharField(default=None, max_length=15, null=True, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')])),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
        ),
        migrations.AddField(
            model_name='podcast',
            name='amazon_music_status',
            field=models.CharField(default=b'inactive', max_length=15, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')]),
        ),
        migrations.AddField(
            model_name='podcast',
            name='apple_podcasts_status',
            field=models.CharField(default=b'inactive', max_length=15, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')]),
        ),
        migrations.AddField(
            model_name='podcast',
            name='google_podcasts_status',
            field=models.CharField(default=b'inactive', max_length=15, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')]),
        ),
        migrations.AddField(
            model_name='podcast',
            name='podcast_addict_status',
            field=models.CharField(default=b'inactive', max_length=15, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')]),
        ),
        migrations.AddField(
            model_name='podcast',
            name='spotify_status',
            field=models.CharField(default=b'inactive', max_length=15, choices=[(b'inactive', b'Inactive'), (b'requested', b'Requested'), (b'active', b'Active'), (b'recheck', b'Recheck')]),
        ),
        migrations.AddField(
            model_name='poddistributionlog',
            name='podcast',
            field=models.ForeignKey(to='log_app.Podcast'),
        ),
        migrations.AddField(
            model_name='poddistributionlog',
            name='triggered_by',
            field=models.ForeignKey(to='log_app.Contact'),
        ),
    ]
