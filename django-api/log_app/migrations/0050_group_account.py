# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations
from django.conf import settings


class Migration(migrations.Migration):

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
        ('log_app', '0049_auto_20160909_0817'),
    ]

    operations = [
        migrations.CreateModel(
            name='Group_account',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('language', models.CharField(default=b'en', max_length=2, choices=[(b'en', b'English'), (b'am', b'Amharic'), (b'sw', b'Kiswahili'), (b'fr', b'Francais')])),
                ('members', models.ManyToManyField(to='log_app.Presenter', null=True, blank=True)),
                ('radio_station', models.ForeignKey(to='log_app.RadioStation', null=True)),
                ('user', models.ForeignKey(to=settings.AUTH_USER_MODEL)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
    ]
