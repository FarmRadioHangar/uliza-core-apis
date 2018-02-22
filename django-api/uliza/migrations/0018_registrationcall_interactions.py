# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0017_auto_20171122_1102'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='interactions',
            field=models.TextField(null=True),
        ),
    ]
