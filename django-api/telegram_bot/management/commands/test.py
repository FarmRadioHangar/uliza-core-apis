from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
from api_core.settings import DJANGO_TELEGRAMBOT
import requests
from log_app.models import *

class Command(BaseCommand):
    def handle(self, *args, **options):
        # id = update.callback_query.data.split('_')[3]
        import datetime
        today = datetime.date.today()
        id = 1
        country = Country.objects.get(id=id)
        programs = Program.objects.filter(end_date__gte=today,radio_station__country=country)
        radio_stations = []
        projects = []

        for program in programs:
            if not program.radio_station.id in radio_stations:
                radio_stations.append(program.radio_station.id)

            if not program.project in projects:
                projects.append(program.project)

        radio_stations= len(radio_stations)
        projects = len(projects)
        programs = len(programs)

        print programs,projects,radio_stations
