# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('auth', '0001_initial'),
        ('log_app', '0035_country_language'),
    ]

    operations = [
        migrations.CreateModel(
            name='Account',
            fields=[
            ],
            options={
                'proxy': True,
            },
            bases=('auth.user',),
        ),
    ]
