# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0075_auth0user'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='contact',
            name='user_id',
        ),
        migrations.AddField(
            model_name='contact',
            name='email',
            field=models.EmailField(max_length=254, verbose_name='email address', blank=True),
        ),
    ]
