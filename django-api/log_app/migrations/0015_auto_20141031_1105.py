# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0014_program_journalists'),
    ]

    operations = [
        migrations.AddField(
            model_name='presenter',
            name='radio_station',
            field=models.ForeignKey(to='log_app.RadioStation', null=True),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='log',
            name='presenter',
            field=models.ForeignKey(to='log_app.Presenter', null=True),
        ),
    ]
