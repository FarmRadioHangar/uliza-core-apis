# uncompyle6 version 3.1.2
# Python bytecode 2.7 (62211)
# Decompiled from: Python 2.7.10 (default, Feb  7 2017, 00:08:15)
# [GCC 4.2.1 Compatible Apple LLVM 8.0.0 (clang-800.0.34)]
# Embedded file name: /Users/jigsa/code/uliza-core-apis/django-api/log_app/management/commands/legacy_formats.py
# Compiled at: 2018-04-18 12:18:23
from log_app.models import Checklist,Format
from django.core.management.base import BaseCommand, CommandError
from django.utils import translation
from django.core.exceptions import ObjectDoesNotExist

class Command(BaseCommand):

    def handle(self, *args, **options):
        checklists = Checklist.objects.all()
        for c in checklists:
            format = Format.objects.get(id = c.radio_format.id + 10)
            c.radio_format = format
            c.save()
