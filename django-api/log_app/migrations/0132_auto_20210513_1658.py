# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0131_auto_20210422_0746'),
    ]

    operations = [
        migrations.CreateModel(
            name='PollSegment',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('index', models.IntegerField()),
                ('episode_number', models.IntegerField()),
                ('title', models.TextField(null=True, blank=True)),
                ('type', models.CharField(default=b'closed', max_length=6, choices=[(b'open', b'open'), (b'closed', b'closed')])),
                ('result', models.TextField(null=True, blank=True)),
                ('poll_file', models.FileField(null=True, upload_to=b'', blank=True)),
                ('poll_file_saved', models.BooleanField(default=True)),
                ('poll_file_offset', models.PositiveIntegerField(default=0)),
                ('last_updated_at', models.DateTimeField(auto_now=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
            ],
        ),
        migrations.AddField(
            model_name='program',
            name='total_polling_responses',
            field=models.IntegerField(default=0),
        ),
        migrations.AddField(
            model_name='program',
            name='unique_polling_respondents',
            field=models.IntegerField(default=0),
        ),
        migrations.AddField(
            model_name='pollsegment',
            name='program',
            field=models.ForeignKey(to='log_app.Program'),
        ),
    ]
