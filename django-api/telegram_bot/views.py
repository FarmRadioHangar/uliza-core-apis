from django.shortcuts import render
from django.http import HttpResponse,HttpResponseForbidden
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
import requests

from telegram.ext import Updater, CommandHandler

"""
{u'message': {u'from': {u'username': u'jixat', u'first_name': u'Jigsa', u'last_name': u'Tesfaye', u'is_bot': False, u'language_code': u'en', u'id': 222282720}, u'text': u'/start', u'entities': [{u'length': 6, u'type': u'bot_command', u'offset': 0}], u'chat': {u'username': u'jixat', u'first_name': u'Jigsa', u'last_name': u'Tesfaye', u'type': u'private', u'id': 222282720}, u'date': 1554066646, u'message_id': 15}, u'update_id': 632340368}

{u'update_id': 632340394, u'callback_query': {u'data': u'/choose_countries', u'message': {u'date': 1554199261, u'text': u'Hi Jigsa', u'from': {u'username': u'FRI_Uliza_bot', u'first_name': u'Uliza Bot', u'is_bot': True, u'id': 890604915}, u'message_id': 41, u'chat': {u'username': u'jixat', u'first_name': u'Jigsa', u'last_name': u'Tesfaye', u'type': u'private', u'id': 222282720}}, u'from': {u'username': u'jixat', u'first_name': u'Jigsa', u'last_name': u'Tesfaye', u'is_bot': False, u'language_code': u'en', u'id': 222282720}, u'id': u'954697015345115007', u'chat_instance': u'-4501186629865900567'}}
"""
#
def start(bot, update):
    update.message.reply_text(
        'Hello {}'.format(update.message.from_user.first_name)
    )
def main(request):
    updater = Updater(TELEGRAM_TOKEN)
    updater.dispatcher.add_handler(CommandHandler('start',start))

    updater.start_polling()
    updater.idle()



def activate(request):
    url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/setWebhook"

    response = requests.post(url,{"url":"https://dev.uliza.fm/api/v1/telegram"},
                  files=dict(certificate=BASE_DIR+"/fullchain.pem"))

    return HttpResponse((response.status_code,response.content))

	# import json
	# data =  json.loads(request.body)
	# print data
	# if 'callback_query' in data:
	# 	url = 'https://api.telegram.org/bot'+TELEGRAM_TOKEN+'/editMessageText'
	# 	response_json = {'chat_id':data['callback_query']['message']['chat']['id'],'message_id':data['callback_query']['message']['message_id'],'text':'Choose countries','reply_markup':json.dumps({'inline_keyboard':[[{'text':'Ethiopia','callback_data':'/choose_countries'},{'text':'Kenya','callback_data':'/my_subscriptions'}]]})}
	# 	response = requests.post(url,data=response_json)
    #
	# else:
	# 	url = "https://api.telegram.org/bot"+TELEGRAM_TOKEN+"/sendMessage"
	# 	#response_json = {'chat_id':data['message']['chat']['id'],'text':'Hi '+data['message']['chat']['first_name'],'reply_markup':{'keyboard':[[{'text':'Choose countries'}],[{'text':'My subscriptions'}]]}}
	# 	response_json = {'chat_id':data['message']['chat']['id'],'text':'Hi '+data['message']['chat']['first_name'],'reply_markup':json.dumps({'inline_keyboard':[[{'text':'Choose countries','callback_data':'/choose_countries'},{'text':'My subscriptions','callback_data':'/my_subscriptions'}]]})}
	# 	response = requests.post(url,data=response_json)
    #
	# print (response.content,response.status_code)
	# return HttpResponse('Success')
    #
    #
