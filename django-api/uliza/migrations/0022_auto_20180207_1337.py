# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0021_auto_20180203_0914'),
    ]

    operations = [
        migrations.RenameField(
            model_name='registrationcall',
            old_name='scheduled_time',
            new_name='schedule_time',
        ),
    ]
