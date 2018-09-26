# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def change_comment_user_to_contact(apps,schema_editor):
    Contact = apps.get_model('log_app','Contact')
    Auth0User = apps.get_model('log_app','Auth0User')
    Comment = apps.get_model('log_app','Comment')

    comments = Comment.objects.all()

    for comment in comments:
        user = Auth0User.objects.filter(username = comment.user.username)

        if user:
            contact = Contact.objects.filter(user_id='local|'+str(user[0].id))
            comment.contact = contact[0]
            comment.save()

class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0084_contact_email'),
    ]

    operations = [
        migrations.AddField(
            model_name='comment',
            name='contact',
            field=models.ForeignKey(to='log_app.Contact', null=True),
        ),
        migrations.RunPython(change_comment_user_to_contact)
    ]
