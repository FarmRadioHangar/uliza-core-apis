# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0016_auto_20171122_1050'),
    ]

    operations = [
        migrations.RenameField(
            model_name='registrationcall',
            old_name='voto_id',
            new_name='voto_call_id',
        ),
    ]
