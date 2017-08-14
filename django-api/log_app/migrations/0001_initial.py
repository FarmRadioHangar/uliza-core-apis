# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='Log',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('topic', models.CharField(max_length=50)),
                ('focus_statement', models.TextField()),
                ('duration', models.IntegerField()),
                ('studio_interviews', models.BooleanField(default=False)),
                ('field_interviews', models.BooleanField(default=False)),
                ('panel', models.BooleanField(default=False)),
                ('community_discussion', models.BooleanField(default=False)),
                ('phone_in', models.BooleanField(default=False)),
                ('vox_pop', models.BooleanField(default=False)),
                ('mini_documentary', models.BooleanField(default=False)),
                ('talk_tape', models.BooleanField(default=False)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('presenter', models.ForeignKey(to=settings.AUTH_USER_MODEL)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='Program',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=50)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='Project',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=50)),
                ('doner', models.CharField(max_length=50)),
                ('focus', models.CharField(max_length=50)),
                ('end_date', models.CharField(max_length=50)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='RadioStation',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=50)),
                ('lattitude', models.FloatField()),
                ('longitude', models.FloatField()),
                ('frequency', models.CharField(max_length=100)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.AddField(
            model_name='program',
            name='project',
            field=models.ForeignKey(to='log_app.Project'),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='radio_station',
            field=models.ForeignKey(to='log_app.RadioStation'),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='log',
            name='program',
            field=models.ForeignKey(to='log_app.Program'),
            preserve_default=True,
        ),
    ]
