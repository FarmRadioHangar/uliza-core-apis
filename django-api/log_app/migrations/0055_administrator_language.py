# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0054_knowledge_partner_country'),
    ]

    operations = [
        migrations.AddField(
            model_name='administrator',
            name='language',
            field=models.CharField(default=b'en', max_length=2, choices=[(b'en', b'English'), (b'am', b'Amharic'), (b'sw', b'Kiswahili'), (b'fr', b'Francais')]),
            preserve_default=True,
        ),
    ]
