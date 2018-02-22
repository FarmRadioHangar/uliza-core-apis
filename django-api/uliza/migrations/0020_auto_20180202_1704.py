# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0019_registrationcall_subscriber'),
    ]

    operations = [
        migrations.RenameField(
            model_name='registrationcall',
            old_name='subscriber',
            new_name='participant',
        ),
        migrations.RemoveField(
            model_name='participant',
            name='registration_call',
        ),
    ]
