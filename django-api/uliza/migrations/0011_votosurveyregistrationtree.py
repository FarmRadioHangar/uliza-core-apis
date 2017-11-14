# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0010_registrationcall_voto_id'),
    ]

    operations = [
        migrations.CreateModel(
            name='VotoSurveyRegistrationTree',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('voto_survey_id', models.IntegerField(unique=True, null=True)),
                ('voto_tree_id', models.IntegerField(null=True)),
            ],
        ),
    ]
