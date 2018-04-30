# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0088_remove_contact_is_active'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='blocked',
            field=models.BooleanField(default=False),
        ),
    ]
