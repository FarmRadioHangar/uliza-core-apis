# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0102_country_gbb'),
    ]

    operations = [
        migrations.AlterField(
            model_name='contact',
            name='role',
            field=models.CharField(max_length=64, null=True, choices=[(b'unknown', b'Unknown'), (b'staff', b'Staff'), (b'consultant', b'Consultant'), (b'broadcaster', b'Broadcaster'), (b'project_partner', b'Project Partner'), (b'knowledge_partner', b'Knowledge Partner')]),
        ),
    ]
