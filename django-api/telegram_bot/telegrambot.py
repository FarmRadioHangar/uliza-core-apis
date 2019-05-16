# -*- coding: utf-8 -*-
from telegram.ext import CommandHandler,ConversationHandler, Filters, CallbackQueryHandler,RegexHandler ,MessageHandler, Filters
from django_telegrambot.apps import DjangoTelegramBot
from django.template.loader import render_to_string
from log_app.models import *
from telegram_bot.callbacks.programs import *
from telegram_bot.callbacks.comments import *
from telegram_bot.callbacks.radio_stations import *
from telegram_bot.callbacks.projects import *

import logging
logging.basicConfig()
logger = logging.getLogger('django')
exclude_country_id = [4]

def flag(code):
    OFFSET = 127397
    points = [ord(x) + OFFSET for x in code.upper()]

    return ("\\U%08x\\U%08x" % tuple(points)).decode("unicode-escape")


def start(bot, update):
    bot.sendMessage(update.message.chat_id, text='This bot will help you find data related to projects from Farm Radio International. farmradio.org',reply_markup=
                    {'inline_keyboard':[[{'text':'Choose country','callback_data':'/country'}],
                                        [{'text':'My subscriptions','callback_data':'/my_subscriptions'}]
                                        ]})

def country(bot, update):
    countries = Country.objects.exclude(id__in=exclude_country_id)
    reply_markup = []
    row_content = []
    column_count = 0
    index = -1
    init = True

    for country in countries:
        f = flag(country.country_code)
        data = {"text":f+" "+country.name,'callback_data':'/country_chosen_'+str(country.id)}

        if column_count == 3 or init:
            init=False
            row_content = []
            row_content.append(data)
            index = index+1
            reply_markup.append(row_content)
            column_count=0
        else:
            row_content.append(data)
            reply_markup[index] = row_content

        column_count=column_count+1

    bot.editMessageText("Choose country",chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})

def country_chosen(bot, update):
    print update
    if update.message:
        id = update.message.text.split("/go_back_to_country_profile")[1]
    else:
        id = update.callback_query.data.split('_')[2]

    import datetime
    today = datetime.date.today()

    country = Country.objects.get(id=id)
    programs = Program.objects.filter(radio_station__country=country)
    projects = Project.objects.filter(country=country)
    active_projects = projects.filter(end_date__gte=today,start_date__lt=today)
    stations = []
    active_stations = []
    active_programs = []

    for program in programs:
        if program.end_date >= today:
            active_programs.append(program.id)

        if not program.radio_station.id in stations:
            stations.append(program.radio_station.id)

        if program.id in active_programs and not program.radio_station.id in active_stations:
            active_stations.append(program.radio_station.id)


    active_stations= len(active_stations)
    active_projects = len(active_projects)
    active_programs = len(active_programs)
    stations= len(stations)
    projects = len(projects)
    programs = len(programs)
    country_command_name = country.name.replace(' ','_')

    output = render_to_string('country_profile.html',
                              context={'country_flag':flag(country.country_code),
                                       'stations':stations,
                                       'programs':programs,
                                       'projects':projects,
                                       'active_stations':active_stations,
                                       'active_programs':active_programs,
                                       'active_projects':active_projects,
                                       'country_command_name':country_command_name,
                                       'country_name':country.name})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML')
    else:
        bot.sendMessage(update.callback_query.message.chat_id,
                    text=output,
                    parse_mode='HTML')

def my_subscriptions(bot, update):
    bot.answer_callback_query(update.callback_query.id, text='SUBSCRIPTIONS')

# sendAudio(chat_id, audio, duration=None, performer=None, title=None, caption=None, disable_notification=False, reply_to_message_id=None, reply_markup=None, timeout=20, parse_mode=None, thumb=None, **kwargs)

def error(bot, update, error):
    logger.warn('Update "%s" caused error "%s"' % (update, error))


def main():
    logger.info("Loading handlers for telegram bot")

    dp = DjangoTelegramBot.dispatcher

    dp.add_handler(CommandHandler(["start","home"], start))

    # programs
    dp.add_handler(RegexHandler("/list_all_programs_in*", list_all_programs_in_country))
    dp.add_handler(CallbackQueryHandler(list_all_programs_in_country,pattern="/list_all_programs_in*"))
    dp.add_handler(RegexHandler("/list_active_programs_in*", list_active_programs_in_country))
    dp.add_handler(CallbackQueryHandler(list_active_programs_in_country,pattern="/list_active_programs_in*"))
    dp.add_handler(RegexHandler("/see_program_details_PID*", program_details))
    dp.add_handler(RegexHandler("/play_episode__*", program_episode))

    #comments
    comment_handler = ConversationHandler(
        entry_points=[CallbackQueryHandler(comment_instruction,pattern='/add_comment*')],
        states = {0: [CallbackQueryHandler(comment_instruction,pattern='/add_comment*')],
                  1: [MessageHandler(Filters.text,add_comment)]},
        fallbacks = [CommandHandler('cancel', start)]
    )
    dp.add_handler(comment_handler)
    dp.add_handler(CallbackQueryHandler(show_comments,pattern='/show_comments*'))
    dp.add_handler(RegexHandler("/delete_comment__*", delete_comment))

    #radio_stations
    dp.add_handler(RegexHandler("/list_all_stations_in*", list_all_stations_in_country))
    dp.add_handler(CallbackQueryHandler(list_all_stations_in_country,pattern="/list_all_stations_in*"))
    dp.add_handler(RegexHandler("/list_active_stations_in*", list_active_stations_in_country))
    dp.add_handler(CallbackQueryHandler(list_active_stations_in_country,pattern="/list_active_stations_in*"))
    dp.add_handler(RegexHandler("/see_station_details_RSID*", station_details))


    #projects
    dp.add_handler(RegexHandler("/list_all_projects_in*", list_all_projects_in_country))
    dp.add_handler(CallbackQueryHandler(list_all_projects_in_country,pattern="/list_all_projects_in*"))
    dp.add_handler(RegexHandler("/list_active_projects_in*", list_active_projects_in_country))
    dp.add_handler(CallbackQueryHandler(list_active_projects_in_country,pattern="/list_active_projects_in*"))
    dp.add_handler(RegexHandler("/see_project_details_PID*", project_details))

    dp.add_handler(CommandHandler("home", start))
    dp.add_handler(CallbackQueryHandler(country_chosen,pattern="/country_chosen*"))
    dp.add_handler(RegexHandler("/go_back_to_country_profile*", country_chosen))
    dp.add_handler(CallbackQueryHandler(my_subscriptions,pattern="/my_subscriptions"))
    dp.add_handler(CallbackQueryHandler(country,pattern="/country"))

    dp.add_error_handler(error)
