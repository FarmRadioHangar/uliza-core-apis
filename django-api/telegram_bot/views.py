from django.shortcuts import render
from django.http import HttpResponse,HttpResponseForbidden


from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
import requests

def activate(request):
    url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/setWebhook"

    response = requests.post(url,{"url":"https://dev.uliza.fm/api/v1/telegram"},
                  files=dict(certificate=BASE_DIR+"/fullchain.pem"))

    return HttpResponse(response.content)

def start(request):
    return HttpResponse('This is FRI speaking... what can I help you?')
