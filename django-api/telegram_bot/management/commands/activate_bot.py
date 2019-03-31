from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
import requests

class Command(BaseCommand):
    def handle(self, *args, **options):
        url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/setWebhook"

        response = requests.post(url,{"url":"https://dev.uliza.fm/api/v1/telegram"},
                      files=dict(certificate=BASE_DIR+"/fullchain.pem"))

        import pdb; pdb.set_trace()
        print response
