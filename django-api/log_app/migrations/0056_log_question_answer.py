# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0055_administrator_language'),
    ]

    operations = [
        migrations.AddField(
            model_name='log',
            name='question_answer',
            field=models.TextField(null=True, blank=True),
            preserve_default=True,
        ),
    ]
