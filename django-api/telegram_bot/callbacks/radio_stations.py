from log_app.models import *
from django.template.loader import render_to_string


def station_details(bot,update):
    station_id = update.message.text.split("/see_station_details_RSID")[1]
    station = RadioStation.objects.get(id = station_id)
    programs = Program.objects.filter(radio_station=station)

    output = render_to_string('stations_details.html',context={'station':station,'programs':programs})

    bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML')

def list_all_stations_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_all_stations_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_all_stations_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]


    country = Country.objects.get(name=country_name.replace('_',' '))
    stations = RadioStation.objects.filter(country__name=country_name.replace('_',' '))

    from django.core.paginator import Paginator
    stations = Paginator(stations,10)

    callback_data = '/list_all_stations_in_'+country_name
    reply_markup=[[]]
    stations = stations.page(page)

    if stations.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(stations.previous_page_number())})

    if stations.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(stations.next_page_number())})

    stations = stations.object_list

    output = render_to_string('stations_list.html',context={'stations':stations,'title':'Radio stations in','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})


def list_active_stations_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_active_stations_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_active_stations_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]


    import datetime
    today = datetime.date.today()

    country = Country.objects.get(name=country_name.replace('_',' '))
    programs = Program.objects.filter(radio_station__country__name=country_name.replace('_',' '),end_date__gte=today).values_list('radio_station__id',flat=True)
    stations = RadioStation.objects.filter(pk__in=programs)

    from django.core.paginator import Paginator
    stations = Paginator(stations,10)

    callback_data = '/list_active_stations_in_'+country_name
    reply_markup=[[]]
    stations = stations.page(page)

    if stations.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(stations.previous_page_number())})

    if stations.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(stations.next_page_number())})

    stations = stations.object_list

    output = render_to_string('stations_list.html',context={'stations':stations,'title':'Active radio stations','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})
