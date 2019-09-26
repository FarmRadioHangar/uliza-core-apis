# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0116_program_media_backup_status'),
    ]

    operations = [
        migrations.AlterField(
            model_name='program',
            name='media_backup_status',
            field=models.CharField(default=b'none', max_length=15, choices=[(b'none', b'None'), (b'removed', b'Removed'), (b'zip', b'Zip')]),
        ),
    ]
