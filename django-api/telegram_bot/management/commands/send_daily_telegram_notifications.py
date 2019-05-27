from django.core.management.base import BaseCommand, CommandError
from api_core.settings import BASE_DIR,TELEGRAM_TOKEN
from api_core.settings import DJANGO_TELEGRAMBOT
import requests
from log_app.models import Log,Format
from telegram_bot.models import ProgramSubscription
from django.template.loader import render_to_string
from telegram import Bot
import datetime

def episodes_aired(program):
    from django.utils import timezone
    today = timezone.now()
    start_date = program.start_date

    if today < start_date:
        today = start_date
    weeks_left = program.end_date - today.date()
    weeks_left = weeks_left.days/7 + 1

    if weeks_left < 1:
        weeks_left = 0

    return program.weeks - weeks_left

class Command(BaseCommand):
    def handle(self, *args, **options):
        today_min = datetime.datetime.combine(datetime.date.today(), datetime.time.min)
        today_max = datetime.datetime.combine(datetime.date.today(), datetime.time.max)
        logs = Log.objects.filter(created_at__range=(today_min,today_max))
        program_notifications = {}

        for log in logs:
            if not str(log.program.id) in program_notifications:
                program_notifications[str(log.program.id)] = [log]
            else:
                program_notifications[str(log.program.id)].append(log)

        print program_notifications
        bot = Bot(TELEGRAM_TOKEN)
        for program_id in program_notifications.keys():
            # need the key as program_id
            subscribers = ProgramSubscription.objects.filter(programs=program_id)

            for subscriber in subscribers:
                # send with chat_id
                print "Send to "+str(subscriber.chat_id)
                # by notification meaning LOG
                for log in program_notifications[str(program_id)]:
                    print "LogId "+str(log.id)

                    formats = Format.objects.filter(id__in=log.formats.values_list('id',flat=True))
                    aired_episodes = episodes_aired(log.program)
                    reply_markup=[[{'text':'Comment','callback_data':'/add_comment__'+str(log.id)},{'text':'Show my comments','callback_data':'/show_comments__'+str(log.id)}]]

                    if log.recording_backup:
                        link = 'https://log.uliza.fm'+log.recording_backup.url
                        # link = 'https://log.uliza.fm/media/Uliza-log-Wolayta%20CA%202018%202nd%20Phase-18.mp3'
                        if log.offset < 200001:
                            output = render_to_string('episode_caption.html',context={'program':log.program,'log':log,'formats':formats,'aired_episodes':aired_episodes,'glink':None,'link':None,'notification':True})
                            bot.sendAudio(subscriber.chat_id,link,caption=output,parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})
                        else:
                            output = render_to_string('episode_caption.html',context={'program':log.program,'log':log,'formats':formats,'aired_episodes':aired_episodes,'glink':None,'link':link,'notification':True})
                            bot.sendMessage(subscriber.chat_id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})
                    elif log.gdrive_available:
                        glink = 'https://log.uliza.fm/api/v1/logs/recording/gdrive/'+str(log.id)
                        output = render_to_string('episode_caption.html',context={'program':log.program,'log':log,'formats':formats,'aired_episodes':aired_episodes,'glink':glink,'notification':True})
                        bot.sendMessage(subscriber.chat_id,text=output,parse_mode='HTML')
                    else:
                        glink = None
                        output = render_to_string('episode_caption.html',context={'program':log.program,'log':log,'formats':formats,'aired_episodes':aired_episodes,'glink':glink,'notification':True})
                        bot.sendMessage(subscriber.chat_id,text=output,parse_mode='HTML')
