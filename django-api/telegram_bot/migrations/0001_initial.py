# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0112_comment_telegram_username'),
    ]

    operations = [
        migrations.CreateModel(
            name='ProgramSubscription',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('username', models.CharField(max_length=80)),
                ('chat_id', models.CharField(max_length=100)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('programs', models.ManyToManyField(to='log_app.Program', blank=True)),
            ],
        ),
    ]
