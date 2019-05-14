from log_app.models import *
from django.template.loader import render_to_string


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

def program_episode(bot, update):
    # from telegram import InputMediaAudio
    link =  "https://log.uliza.fm/media/Uliza-log-PRTV-REGI-GiZ-1.mp3"

    id = update.message.text.split("/play_episode__")[1]
    log = Log.objects.get(pk=id)
    formats = Format.objects.filter(id__in=log.formats.values_list('id',flat=True))
    aired_episodes = episodes_aired(log.program)
    caption = render_to_string('episode_caption.html',context={'program':log.program,'log':log,'formats':formats,'aired_episodes':aired_episodes})
    reply_markup=[[{'text':'Comment','callback_data':'/add_comment__'+str(log.id)},{'text':'See comments','callback_data':'/see_comments__'+str(log.id)}]]

    bot.sendAudio(update.message.chat_id,link,caption=caption,parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def program_details(bot,update):
    program_id = update.message.text.split("/see_program_details_PID")[1]
    program = Program.objects.get(id = program_id)
    logs = Log.objects.filter(program=program)
    aired_episodes = episodes_aired(program)

    output = render_to_string('programs_details.html',context={'program':program,'logs':logs,'aired_episodes':aired_episodes})

    bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML')


def list_all_programs_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_all_programs_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_all_programs_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]


    country = Country.objects.get(name=country_name.replace('_',' '))
    programs = Program.objects.filter(radio_station__country__name=country_name.replace('_',' ')).order_by('-end_date')

    from django.core.paginator import Paginator
    programs = Paginator(programs,10)

    callback_data = '/list_all_programs_in_'+country_name
    reply_markup=[[]]
    programs = programs.page(page)

    if programs.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(programs.previous_page_number())})

    if programs.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(programs.next_page_number())})

    programs = programs.object_list

    output = render_to_string('programs_list.html',context={'programs':programs,'title':'Radio programs in','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})


def list_active_programs_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_active_programs_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_active_programs_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]

    import datetime
    today = datetime.date.today()

    country = Country.objects.get(name=country_name.replace('_',' '))
    programs = Program.objects.filter(radio_station__country__name=country_name.replace('_',' '),end_date__gte=today).order_by('-end_date')

    from django.core.paginator import Paginator
    programs = Paginator(programs,10)

    callback_data = '/list_all_programs_in_'+country_name
    reply_markup=[[]]
    programs = programs.page(page)

    if programs.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(programs.previous_page_number())})

    if programs.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(programs.next_page_number())})

    programs = programs.object_list

    output = render_to_string('programs_list.html',context={'programs':programs,'title':'Active radio programs in','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})
