from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
from api_core.settings import DJANGO_TELEGRAMBOT
import requests

class Command(BaseCommand):
    def handle(self, *args, **options):
        url = DJANGO_TELEGRAMBOT['WEBHOOK_SITE']+DJANGO_TELEGRAMBOT['WEBHOOK_PREFIX']+TELEGRAM_TOKEN+'/'
        text = '/start'

        response = requests.post(url,{"update_id":'0111',"message":'/start'})

        print response.content
