# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0087_contact_notify_on_log_create'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='contact',
            name='is_active',
        ),
    ]
