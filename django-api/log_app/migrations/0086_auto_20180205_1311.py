# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def add_superuser_data(apps, schema_editor):
    Auth0User = apps.get_model('log_app','Auth0User')
    Contact = apps.get_model('log_app','Contact')
    users = Auth0User.objects.filter(is_super_user=True)

    for user in users:
        c = Contact.objects.filter(user_id='local|'+str(user.id))

        if c:
            c = c[0]
            c.is_superuser = True
            c.save()


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
        migrations.RunPython(add_superuser_data),
    ]
