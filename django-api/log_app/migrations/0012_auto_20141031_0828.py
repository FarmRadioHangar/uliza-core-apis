# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0011_auto_20141031_0713'),
    ]

    operations = [
        migrations.CreateModel(
            name='Program_presenters',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('role', models.CharField(max_length=64)),
                ('presenter', models.ForeignKey(to='log_app.Presenter')),
                ('program', models.ForeignKey(to='log_app.Program')),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.RemoveField(
            model_name='presenter',
            name='program',
        ),
        migrations.RemoveField(
            model_name='presenter',
            name='role',
        ),
        migrations.AddField(
            model_name='presenter',
            name='email',
            field=models.EmailField(max_length=50, null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='presenter',
            name='phone_number',
            field=models.CharField(max_length=50, null=True, blank=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='program',
            name='journalists',
            field=models.ManyToManyField(to='log_app.Presenter', through='log_app.Program_presenters'),
            preserve_default=True,
        ),
    ]
