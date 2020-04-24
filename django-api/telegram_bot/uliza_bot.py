from log_app.models import *
from django.template.loader import render_to_string

def flag(code):
    OFFSET = 127397
    points = [ord(x) + OFFSET for x in code.upper()]

    return ("\\U%08x\\U%08x" % tuple(points)).decode("unicode-escape")


def start(bot, update):
    bot.sendMessage(update.message.chat_id, text='This bot will help you find information related to projects from Farm Radio International. farmradio.org',reply_markup=
                    {'inline_keyboard':[[{'text':'Choose country','callback_data':'/country'}],
                                        [{'text':'Uniterra programs','callback_data':'/uniterra'}],
                                        [{'text':'My subscriptions','callback_data':'/my_subscriptions'}]
                                        ]})

def country(bot, update):
    countries = Country.objects.exclude(name__startswith='*')
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
    chat_id = update.callback_query.message.chat.id
    subscriptions = ProgramSubscription.objects.filter(chat_id=chat_id)
    output = '\n~\n'
    if subscriptions:
        if not subscriptions[0].programs.all():
            output = 'No subscriptions'
        else:
            for p in subscriptions[0].programs.all():
                output = output + '- {}  [/remove_subscription_{}]\n\n'.format(p.name,p.pk)
            output= output+'/home'
    else:
        output = 'No subscriptions'

    bot.sendMessage(update.callback_query.message.chat_id, text=output)


def delete_subscription(bot, update):
    program_id = update.message.text.split("/remove_subscription_")[1]
    program = Program.objects.get(pk=program_id)
    chat_id = update.message.chat_id
    subscriptions = ProgramSubscription.objects.filter(chat_id=chat_id)
    if subscriptions:
        subscriptions[0].programs.remove(str(program_id))

    bot.sendMessage(update.message.chat_id, text="Removed subscription! > "+str(program.name))

# sendAudio(chat_id, audio, duration=None, performer=None, title=None, caption=None, disable_notification=False, reply_to_message_id=None, reply_markup=None, timeout=20, parse_mode=None, thumb=None, **kwargs)
