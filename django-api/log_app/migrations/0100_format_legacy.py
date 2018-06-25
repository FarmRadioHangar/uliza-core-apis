# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations

def migrate_legacy_format(apps, schema_editor):
    legacy_format_names = [
         ('studio_interviews', 'Studio interviews'),
         ('field_interviews', 'Field interviews'),
         ('panel', 'Panel'),
         ('community_discussion', 'Community discussion'),
         ('phone_in', 'Phone-in'),
         ('vox_pop', 'Vox Pop'),
         ('mini_documentary', 'Min documentary'),
         ('talk_tape', 'Talk tape'),
         ('question_answer', 'Q&A'),
         ('case_study', 'Case study')]

    format_instances = {}
    Format = apps.get_model('log_app','Format')
    Log = apps.get_model('log_app','Log')

    for format in legacy_format_names:
        instance = Format.objects.filter(name=format[1])

        if not instance:
            instance = Format.objects.create(name=format[1], description=format[0], legacy=True)
        else:
            instance = instance[0]

        format_instances[format[0]] = instance

    logs = Log.objects.all()
    for l in logs:
        for format in legacy_format_names:
            if l.__dict__[format[0]]:
                l.formats.add(format_instances[format[0]])


class Migration(migrations.Migration):

    dependencies = [
        ('log_app', '0099_auto_20180418_0740'),
    ]

    operations = [
        migrations.AddField(
            model_name='format',
            name='legacy',
            field=models.BooleanField(default=False),
        ),
        migrations.RunPython(migrate_legacy_format),
    ]
