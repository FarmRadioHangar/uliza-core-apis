# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0126_radiostation_premium_account'),
    ]

    operations = [
        migrations.CreateModel(
            name='Podcast',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('spreaker_show_id', models.CharField(max_length=120, null=True, blank=True)),
                ('title', models.CharField(max_length=50)),
                ('website', models.CharField(max_length=50, null=True, blank=True)),
                ('owner', models.CharField(max_length=80, null=True, blank=True)),
                ('owner_email', models.EmailField(max_length=50, null=True, blank=True)),
                ('language', models.CharField(default=b'en', max_length=6)),
                ('description', models.TextField(default=b'None', null=True, blank=True)),
                ('category', models.CharField(max_length=60, null=True, blank=True)),
                ('explicit', models.BooleanField(default=False)),
                ('image', models.CharField(max_length=400, null=True, blank=True)),
                ('custom_image', models.CharField(max_length=200, null=True, blank=True)),
                ('twitter_url', models.CharField(max_length=400, null=True, blank=True)),
                ('facebook_url', models.CharField(max_length=400, null=True, blank=True)),
                ('itunes', models.CharField(max_length=100, null=True, blank=True)),
                ('skype_name', models.CharField(max_length=100, null=True, blank=True)),
                ('text_number', models.CharField(max_length=100, null=True, blank=True)),
                ('telephone_number', models.CharField(max_length=50, null=True, blank=True)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('radio_station', models.ForeignKey(to='log_app.RadioStation')),
            ],
        ),
        migrations.CreateModel(
            name='PodEpisode',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('spreaker_episode_id', models.CharField(max_length=120, null=True, blank=True)),
                ('title', models.CharField(max_length=150, null=True, blank=True)),
                ('description', models.TextField(default=b'None', null=True, blank=True)),
                ('audio_file', models.FileField(null=True, upload_to=b'', blank=True)),
                ('audio_saved', models.BooleanField(default=True)),
                ('public', models.BooleanField(default=False)),
                ('audio_file_offset', models.PositiveIntegerField(default=0)),
                ('spreaker_audio_url', models.TextField(null=True, blank=True)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('podcast', models.ForeignKey(to='log_app.Podcast')),
            ],
        ),
        migrations.AddField(
            model_name='format',
            name='project_related',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='format',
            name='projects',
            field=models.ManyToManyField(to='log_app.Project', blank=True),
        ),
    ]
