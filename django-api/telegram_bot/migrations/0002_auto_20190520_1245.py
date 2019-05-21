# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('telegram_bot', '0001_initial'),
    ]

    operations = [
        migrations.AlterField(
            model_name='programsubscription',
            name='programs',
            field=models.ManyToManyField(to='log_app.Program', null=True, blank=True),
        ),
    ]
