# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0089_contact_blocked'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='log',
            name='presenter',
        ),
    ]
