# -*- coding: utf-8 -*-
from api_core.settings import TELEGRAM_TOKENS
from telegram.ext import CommandHandler,ConversationHandler, Filters, CallbackQueryHandler,RegexHandler ,MessageHandler, Filters
from django_telegrambot.apps import DjangoTelegramBot

# callbacks
from telegram_bot.callbacks.programs import *
from telegram_bot.callbacks.comments import *
from telegram_bot.models import ProgramSubscription
from telegram_bot.callbacks.radio_stations import *
from telegram_bot.callbacks.projects import *

import logging
logging.basicConfig()
logger = logging.getLogger('django')

from telegram_bot.uliza_bot import *
from telegram_bot.covid_bot import *

def error(bot, update, error):
    logger.warn('Update "%s" caused error "%s"' % (update, error))


def main():
    logger.info("Loading handlers for telegram bot")

    # covid_dp.add_handler(CommandHandler(["start","home"], covid_start))

    dp = DjangoTelegramBot.getDispatcher(TELEGRAM_TOKENS[0])

    dp.add_handler(CommandHandler(["start","home"], start))

    # programs
    dp.add_handler(RegexHandler("/list_all_programs_in*", list_all_programs_in_country))
    dp.add_handler(CallbackQueryHandler(list_all_programs_in_country,pattern="/list_all_programs_in*"))
    dp.add_handler(RegexHandler("/list_active_programs_in*", list_active_programs_in_country))
    dp.add_handler(CallbackQueryHandler(list_active_programs_in_country,pattern="/list_active_programs_in*"))
    dp.add_handler(RegexHandler("/see_program_details_PID*", program_details))
    dp.add_handler(RegexHandler("/play_episode__*", program_episode))
    dp.add_handler(CallbackQueryHandler(subscribe_to_program,pattern="/subscribe_program*"))
    dp.add_handler(CallbackQueryHandler(podcast,pattern="/podcast_*"))
    dp.add_handler(RegexHandler("/remove_subscription_*", delete_subscription))
    dp.add_handler(CommandHandler("podcast_details", podcast_details))

    #comments
    comment_handler = ConversationHandler(
        entry_points=[CallbackQueryHandler(comment_instruction,pattern='/add_comment*')],
        states = {
                  0: [CallbackQueryHandler(comment_instruction,pattern='/add_comment*')],
                  1: [MessageHandler(Filters.text,add_comment)]},
        fallbacks = [MessageHandler(Filters.text,add_comment)],
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
    dp.add_handler(CallbackQueryHandler(subscribe_to_project,pattern="/subscribe_project*"))
    #uniterra
    dp.add_handler(CallbackQueryHandler(list_all_uniterra_projects,pattern="/uniterra"))

    dp.add_handler(CommandHandler("home", start))
    dp.add_handler(CallbackQueryHandler(country_chosen,pattern="/country_chosen*"))
    dp.add_handler(RegexHandler("/go_back_to_country_profile*", country_chosen))
    dp.add_handler(CallbackQueryHandler(my_subscriptions,pattern="/my_subscriptions"))
    dp.add_handler(CallbackQueryHandler(country,pattern="/country"))

    dp.add_error_handler(error)

    covid_dp = DjangoTelegramBot.getDispatcher(TELEGRAM_TOKENS[1])

    covid_dp.add_handler(CommandHandler(["start","home"], covid_start))
    covid_dp.add_handler(CommandHandler("lang",get_language))
    covid_dp.add_handler(CallbackQueryHandler(covid_start,pattern="/start"))
    covid_dp.add_handler(CallbackQueryHandler(learn,pattern="/learn"))
    covid_dp.add_handler(CallbackQueryHandler(tips_and_resources,pattern="/tips_and_resources"))

    # covid19 content
    covid_dp.add_handler(CallbackQueryHandler(how_virus_is_spread,pattern="/how_the_virus_is_spread"))
    covid_dp.add_handler(CallbackQueryHandler(precautionary_measures,pattern="/precautionary_measures"))
    covid_dp.add_handler(CallbackQueryHandler(symptoms_of_infection,pattern="/symptoms_of_infection"))
    covid_dp.add_handler(CallbackQueryHandler(myths_misinformation,pattern="/myths_misinformation*"))

    #tips and resources
    covid_dp.add_handler(CallbackQueryHandler(tips_and_resources,pattern="/tips_and_resources"))
    covid_dp.add_handler(CallbackQueryHandler(safety_for_broadcasters,pattern="/safety_for_broadcasters"))
    covid_dp.add_handler(CallbackQueryHandler(broadcaster_resources,pattern="/broadcaster_resources"))
    covid_dp.add_handler(CallbackQueryHandler(join_online_groups,pattern="/join_online_groups"))

    covid_dp.add_handler(CallbackQueryHandler(get_confirmation,pattern="/no"))
    covid_dp.add_handler(CallbackQueryHandler(get_confirmation,pattern="/ask_confirmation"))
    covid_dp.add_handler(CallbackQueryHandler(question_instruction,pattern="/question_instruction"))
    covid_dp.add_handler(MessageHandler(Filters.text,conversational_dispatch))
    covid_dp.add_handler(MessageHandler(Filters.voice,conversational_dispatch))
    covid_dp.add_handler(CallbackQueryHandler(get_country,pattern="/country_*"))
    covid_dp.add_handler(CallbackQueryHandler(set_language,pattern="/language_*"))


    covid_dp.add_error_handler(error)
