from django.core.management.base import BaseCommand, CommandError
from django.utils import translation
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN

class Command(BaseCommand):
    def handle(self, *args, **options):
        url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/setWebhook"

        response = requests.post("https://dev.uliza.fm/api/v1/telegram/",
                      file('certificate',BASE_DIR+"/fullchain.pem"))
                      
        print response.content
