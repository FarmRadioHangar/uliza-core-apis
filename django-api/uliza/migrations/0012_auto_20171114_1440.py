# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0011_votosurveyregistrationtree'),
    ]

    operations = [
        migrations.AlterModelTable(
            name='votosurveyregistrationtree',
            table='uliza_voto_survey_registration_tree',
        ),
    ]
