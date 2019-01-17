# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0108_project_image'),
    ]

    operations = [
        migrations.RemoveField(
            model_name='log',
            name='case_study',
        ),
        migrations.RemoveField(
            model_name='log',
            name='community_discussion',
        ),
        migrations.RemoveField(
            model_name='log',
            name='field_interviews',
        ),
        migrations.RemoveField(
            model_name='log',
            name='mini_documentary',
        ),
        migrations.RemoveField(
            model_name='log',
            name='panel',
        ),
        migrations.RemoveField(
            model_name='log',
            name='phone_in',
        ),
        migrations.RemoveField(
            model_name='log',
            name='question_answer',
        ),
        migrations.RemoveField(
            model_name='log',
            name='studio_interviews',
        ),
        migrations.RemoveField(
            model_name='log',
            name='talk_tape',
        ),
        migrations.RemoveField(
            model_name='log',
            name='vox_pop',
        ),
    ]
