# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0045_knowledge_partner_role'),
    ]

    operations = [
        migrations.AddField(
            model_name='program',
            name='uliza',
            field=models.BooleanField(default=False),
            preserve_default=True,
        ),
    ]
