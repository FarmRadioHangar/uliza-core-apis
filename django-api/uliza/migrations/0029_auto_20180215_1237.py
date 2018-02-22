# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0028_registrationcall_voto_survey_call_id'),
    ]

    operations = [
        migrations.AlterField(
            model_name='registrationcall',
            name='schedule_time',
            field=models.DateTimeField(null=True),
        ),
    ]
