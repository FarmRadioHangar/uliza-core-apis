# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('uliza', '0012_auto_20171114_1440'),
    ]

    operations = [
        migrations.AddField(
            model_name='registrationcall',
            name='voto_tree_id',
            field=models.IntegerField(default=111),
            preserve_default=False,
        ),
        migrations.AlterField(
            model_name='registrationcall',
            name='voto_id',
            field=models.IntegerField(default=111),
            preserve_default=False,
        ),
    ]
