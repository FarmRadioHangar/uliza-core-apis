# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('log_app', '0001_initial'),
    ]

    operations = [
        migrations.CreateModel(
            name='Presenter',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('role', models.CharField(max_length=64)),
                ('program', models.ForeignKey(to='log_app.Program')),
                ('user', models.ForeignKey(to=settings.AUTH_USER_MODEL)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
        migrations.AddField(
            model_name='radiostation',
            name='city',
            field=models.CharField(max_length=50, null=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='radiostation',
            name='country',
            field=models.CharField(max_length=50, null=True),
            preserve_default=True,
        ),
        migrations.AddField(
            model_name='radiostation',
            name='email',
            field=models.EmailField(max_length=50, null=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='presenter',
            field=models.ForeignKey(to='log_app.Presenter'),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='lattitude',
            field=models.FloatField(null=True),
        ),
        migrations.AlterField(
            model_name='radiostation',
            name='longitude',
            field=models.FloatField(null=True),
        ),
    ]
