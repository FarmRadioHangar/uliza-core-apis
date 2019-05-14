from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
from api_core.settings import DJANGO_TELEGRAMBOT
import requests

class Command(BaseCommand):
    def handle(self, *args, **options):
        url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/setWebhook"
        webhook_url = DJANGO_TELEGRAMBOT['WEBHOOK_SITE']+DJANGO_TELEGRAMBOT['WEBHOOK_PREFIX']+TELEGRAM_TOKEN+'/'

        response = requests.get(url,params={"url":webhook_url})

        print response.content
