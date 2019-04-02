from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
import requests

class Command(BaseCommand):
    def handle(self, *args, **options):
        url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/getUpdates"

        response = requests.get(url)

        print response.content

