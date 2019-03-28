from django.core.management.base import BaseCommand, CommandError
from django.utils import translation

class Command(BaseCommand):
    def handle(self, *args, **options):
        print "activating bot"
