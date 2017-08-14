# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0032_auto_20150630_1838'),
    ]

    operations = [
        migrations.AddField(
            model_name='presenter',
            name='language',
            field=models.CharField(default=b'en', max_length=2, choices=[(b'en', b'English'), (b'am', b'Amharic')]),
            preserve_default=True,
        ),
    ]
