# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import log_app.storage.gd_storage
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('log_app', '0029_log_recording_backup'),
    ]

    operations = [
        migrations.CreateModel(
            name='Administrator',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('job_description', models.CharField(max_length=80, null=True, blank=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.CreateModel(
            name='Country',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('name', models.CharField(max_length=50)),
                ('country_code', models.CharField(max_length=3)),
            ],
            options={
                'verbose_name_plural': 'Countries',
            },
            bases=(models.Model,),
        ),
        migrations.AddField(
            model_name='administrator',
            name='country',
            field=models.ForeignKey(to='log_app.Country'),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='administrator',
            name='user',
            field=models.ForeignKey(to=settings.AUTH_USER_MODEL),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='project',
            name='country',
            field=models.ForeignKey(blank=True, to='log_app.Country', null=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='recording',
            field=models.FileField(storage=log_app.storage.gd_storage.GoogleDriveStorage(), null=True, upload_to=b'/FRI-LOG', blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='recording_backup',
            field=models.FileField(null=True, upload_to=b'/home/jigsa/fri-log/fri-log/media/', blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='presenter',
            name='phone_number',
            field=models.CharField(max_length=50, unique=True, null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='country',
            field=models.ForeignKey(blank=True, to='log_app.Country', null=True),
            preserve_default=True,
        ),
    ]
