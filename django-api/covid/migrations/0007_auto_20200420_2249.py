# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models
import datetime
from django.utils.timezone import utc


class Migration(migrations.Migration):

    dependencies = [
        ('covid', '0006_question_type'),
    ]

    operations = [
        migrations.AddField(
            model_name='question',
            name='answer',
            field=models.TextField(default='i'),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='question',
            name='answered_by',
            field=models.CharField(max_length=30, null=True, blank=True),
        ),
        migrations.AddField(
            model_name='question',
            name='created_at',
            field=models.DateTimeField(default=datetime.datetime(2020, 4, 20, 22, 49, 8, 531365, tzinfo=utc), auto_now_add=True),
            preserve_default=False,
        ),
        migrations.AddField(
            model_name='question',
            name='last_updated_at',
            field=models.DateTimeField(default=datetime.datetime(2020, 4, 20, 22, 49, 8, 531365, tzinfo=utc), auto_now=True),
            preserve_default=False,
        ),
    ]
