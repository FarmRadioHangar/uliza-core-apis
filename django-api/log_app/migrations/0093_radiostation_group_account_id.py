# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0092_remove_comment_user'),
    ]

    operations = [
        migrations.AddField(
            model_name='radiostation',
            name='group_account_id',
            field=models.CharField(max_length=120, null=True, blank=True),
        ),
    ]
