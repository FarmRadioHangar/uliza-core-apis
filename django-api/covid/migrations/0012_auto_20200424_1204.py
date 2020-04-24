# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0011_content_title'),
    ]

    operations = [
        migrations.AlterField(
            model_name='content',
            name='topic_am',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='content',
            name='topic_en',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
        migrations.AlterField(
            model_name='content',
            name='topic_fr',
            field=models.CharField(max_length=100, null=True, blank=True),
        ),
    ]
