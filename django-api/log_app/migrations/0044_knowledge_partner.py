# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('log_app', '0043_auto_20160226_1900'),
    ]

    operations = [
        migrations.CreateModel(
            name='Knowledge_partner',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('phone_number', models.CharField(max_length=50, unique=True, null=True, blank=True)),
                ('organization', models.CharField(max_length=64, null=True)),
                ('language', models.CharField(default=b'en', max_length=2, choices=[(b'en', b'English'), (b'am', b'Amharic'), (b'sw', b'Kiswahili'), (b'fr', b'Francais')])),
                ('project', models.ForeignKey(to='log_app.Project')),
                ('user', models.ForeignKey(to=settings.AUTH_USER_MODEL)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
    ]
