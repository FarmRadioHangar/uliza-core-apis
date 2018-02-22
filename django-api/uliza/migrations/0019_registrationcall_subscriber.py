# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0018_registrationcall_interactions'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='subscriber',
            field=models.ForeignKey(default=1, to='uliza.Participant'),
            preserve_default=False,
        ),
    ]
