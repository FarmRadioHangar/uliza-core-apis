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
    covid_dp.add_handler(CallbackQueryHandler(covid_main_menu,pattern="/go_back"))
    covid_dp.add_handler(CallbackQueryHandler(learn,pattern="/learn"))
    covid_dp.add_handler(CallbackQueryHandler(set_language,pattern="/language_*"))

    # covid19 content
    covid_dp.add_handler(CallbackQueryHandler(basic_facts,pattern="/basic_facts"))
    covid_dp.add_handler(CallbackQueryHandler(more_basic_facts,pattern="/more_basic_facts"))
    covid_dp.add_handler(CallbackQueryHandler(important_info,pattern="/important_info"))
    covid_dp.add_handler(CallbackQueryHandler(how_the_virus_is_spread,pattern="/how_the_virus_is_spread"))
    covid_dp.add_handler(CallbackQueryHandler(more_how_the_virus_is_spread,pattern="/more_how_the_virus_is_spread"))
    covid_dp.add_handler(CallbackQueryHandler(preventive_measures,pattern="/preventive_measures"))
    covid_dp.add_handler(CallbackQueryHandler(more_preventive_measures,pattern="/more_preventive_measures"))

    #broadcaster resources
    covid_dp.add_handler(CallbackQueryHandler(get_radio_resources,pattern="/get_radio_resources"))
    covid_dp.add_handler(CallbackQueryHandler(working_safely,pattern="/working_safely"))
    covid_dp.add_handler(CallbackQueryHandler(sanitize_your_equipment,pattern="/sanitize_your_equipment"))
    covid_dp.add_handler(CallbackQueryHandler(protect_your_health,pattern="/protect_your_health"))
    covid_dp.add_handler(CallbackQueryHandler(good_radio_resources,pattern="/good_radio_resources"))
    covid_dp.add_handler(CallbackQueryHandler(emergency_programs,pattern="/emergency_programs"))
    covid_dp.add_handler(CallbackQueryHandler(farm_radio_resources,pattern="/farm_radio_resources"))
    covid_dp.add_handler(CallbackQueryHandler(farmer_stories,pattern="/farmer_stories"))
    covid_dp.add_handler(CallbackQueryHandler(key_info_radio_scripts,pattern="/key_info_radio_scripts"))
    covid_dp.add_handler(CallbackQueryHandler(covid_information,pattern="/covid_information"))
    covid_dp.add_handler(CallbackQueryHandler(fri_key_messenges,pattern="/fri_key_messenges"))
    covid_dp.add_handler(CallbackQueryHandler(who_resources,pattern="/who_resources"))

    #fact check myths
    covid_dp.add_handler(CallbackQueryHandler(fact_check_myths,pattern="/fact_check_myths"))
    covid_dp.add_handler(CallbackQueryHandler(truth_behind_myths,pattern="/truth_behind_myths*"))
    covid_dp.add_handler(CallbackQueryHandler(fight_fake_news,pattern="/fight_fake_news"))
    covid_dp.add_handler(CallbackQueryHandler(latest_covid_myths,pattern="/latest_covid_myths"))

    """
    # This was the question module
    # currently commented out
    covid_dp.add_handler(CallbackQueryHandler(get_confirmation,pattern="/no"))
    covid_dp.add_handler(CallbackQueryHandler(get_confirmation,pattern="/ask_confirmation"))
    covid_dp.add_handler(CallbackQueryHandler(question_instruction,pattern="/question_instruction"))
    covid_dp.add_handler(MessageHandler(Filters.text,conversational_dispatch))
    covid_dp.add_handler(MessageHandler(Filters.voice,conversational_dispatch))
    covid_dp.add_handler(CallbackQueryHandler(get_country,pattern="/country_*"))
    """
    covid_dp.add_error_handler(error)
