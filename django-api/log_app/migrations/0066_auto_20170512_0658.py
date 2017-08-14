# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0065_auto_20170511_0907'),
    ]

    operations = [
        migrations.AlterField(
            model_name='administrator',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'English'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='group_account',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'English'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='knowledge_partner',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'English'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
            preserve_default=True,
        ),
        migrations.AlterField(
            model_name='presenter',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'English'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
            preserve_default=True,
        ),
    ]
