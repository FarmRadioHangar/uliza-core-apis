# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0085_comment_contact'),
    ]

    operations = [
        migrations.AddField(
            model_name='contact',
            name='is_active',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='contact',
            name='is_admin',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='contact',
            name='is_superuser',
            field=models.BooleanField(default=False),
        ),
        migrations.AlterField(
            model_name='administrator',
            name='language',
            field=models.CharField(default=b'en', max_length=6, choices=[(b'en', b'English'), (b'pt', b'Portuguese'), (b'am', b'Amharic'), (b'fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='contact',
            name='language',
            field=models.CharField(default=b'en', max_length=6, choices=[(b'en', b'English'), (b'pt', b'Portuguese'), (b'am', b'Amharic'), (b'fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='contact',
            name='organization',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='group_account',
            name='language',
            field=models.CharField(default=b'en', max_length=6, choices=[(b'en', b'English'), (b'pt', b'Portuguese'), (b'am', b'Amharic'), (b'fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='knowledge_partner',
            name='language',
            field=models.CharField(default=b'en', max_length=6, choices=[(b'en', b'English'), (b'pt', b'Portuguese'), (b'am', b'Amharic'), (b'fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='presenter',
            name='language',
            field=models.CharField(default=b'en', max_length=6, choices=[(b'en', b'English'), (b'pt', b'Portuguese'), (b'am', b'Amharic'), (b'fr', b'Francais')]),
        ),
    ]
