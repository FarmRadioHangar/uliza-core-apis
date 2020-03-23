# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0123_auto_20200311_0816'),
    ]

    operations = [
        migrations.AlterField(
            model_name='contact',
            name='role',
            field=models.CharField(max_length=64, null=True, choices=[(b'unknown', b'Unknown'), (b'staff', b'Staff'), (b'consultant', b'Consultant'), (b'broadcaster', b'Broadcaster'), (b'gender_specialist', b'Gender Specialist'), (b'knowledge_partner', b'Knowledge Partner')]),
        ),
    ]
