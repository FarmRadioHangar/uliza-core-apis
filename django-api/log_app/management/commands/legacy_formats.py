# uncompyle6 version 3.1.2
# Python bytecode 2.7 (62211)
# Decompiled from: Python 2.7.10 (default, Feb  7 2017, 00:08:15) 
# [GCC 4.2.1 Compatible Apple LLVM 8.0.0 (clang-800.0.34)]
# Embedded file name: /Users/jigsa/code/uliza-core-apis/django-api/log_app/management/commands/legacy_formats.py
# Compiled at: 2018-04-18 12:18:23
from log_app.models import Format, Log
from django.core.management.base import BaseCommand, CommandError
from django.utils import translation
from django.core.exceptions import ObjectDoesNotExist

class Command(BaseCommand):

    def handle(self, *args, **options):
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
        for format in legacy_format_names:
            try:
                instance = Format.objects.get(name=format[1])
            except ObjectDoesNotExist as e:
                instance = Format.objects.create(name=format[1], description=format[0], legacy=True)

            format_instances[format[0]] = instance

        logs = Log.objects.all()
        for l in logs:
            for format in legacy_format_names:
                if l.__dict__[format[0]]:
                    l.formats.add(format_instances[format[0]])