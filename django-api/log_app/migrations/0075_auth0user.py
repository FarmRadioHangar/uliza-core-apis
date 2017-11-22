# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
import django.core.validators


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0074_auto_20171030_0630'),
    ]

    operations = [
        migrations.CreateModel(
            name='Auth0User',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('username', models.CharField(error_messages={b'unique': 'A user with that username already exists.'}, max_length=30, validators=[django.core.validators.RegexValidator(b'^[\\w.@+-]+$', 'Enter a valid username. This value may contain only letters, numbers and @/./+/-/_ characters.', b'invalid')], help_text='Required. 30 characters or fewer. Letters, digits and @/./+/-/_ only.', unique=True, verbose_name='username')),
                ('password', models.CharField(max_length=128, verbose_name='password')),
                ('role', models.CharField(max_length=50)),
                ('email', models.EmailField(max_length=254, verbose_name='email address', blank=True)),
                ('is_super_user', models.BooleanField(default=False, help_text='Designates whether the user can have access to multiple countries site.', verbose_name='super user')),
                ('notify_on_log_create', models.BooleanField(default=False, help_text='If the user prefers to get notification or not', verbose_name='notify on new log')),
            ],
        ),
    ]
