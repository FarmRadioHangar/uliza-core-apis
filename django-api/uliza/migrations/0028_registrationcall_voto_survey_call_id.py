# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0027_auto_20180215_1124'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='voto_survey_call_id',
            field=models.IntegerField(default=1),
            preserve_default=False,
        ),
    ]
