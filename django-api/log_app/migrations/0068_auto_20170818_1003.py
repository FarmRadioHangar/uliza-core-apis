# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0067_auto_20170815_0945'),
    ]

    operations = [
        migrations.AlterField(
            model_name='administrator',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'Portuguese'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='group_account',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'Portuguese'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='group_account',
            name='members',
            field=models.ManyToManyField(to='log_app.Presenter', blank=True),
        ),
        migrations.AlterField(
            model_name='knowledge_partner',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'Portuguese'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='presenter',
            name='language',
            field=models.CharField(default=b'en-us', max_length=6, choices=[(b'en-us', b'English'), (b'pt-mz', b'Portuguese'), (b'am-et', b'Amharic'), (b'fr-fr', b'Francais')]),
        ),
        migrations.AlterField(
            model_name='program',
            name='access',
            field=models.ManyToManyField(to=settings.AUTH_USER_MODEL, blank=True),
        ),
        migrations.AlterField(
            model_name='program',
            name='journalists',
            field=models.ManyToManyField(to='log_app.Presenter', blank=True),
        ),
        migrations.AlterField(
            model_name='program',
            name='knowledge_partner',
            field=models.ManyToManyField(to='log_app.Knowledge_partner', blank=True),
        ),
    ]
