from log_app.models import *
from telegram_bot.models import ProgramSubscription
from django.template.loader import render_to_string

def project_details(bot,update):
    project_id = update.message.text.split("/see_project_details_PID")[1]
    project = Project.objects.get(id = project_id)
    programs = Program.objects.filter(project=project)
    reply_markup = [[]]
    if programs:
        reply_markup[0].append({'text':'Subscribe to all programs in this project','callback_data':'/subscribe_project_'+str(project_id)})

    output = render_to_string('projects_details.html',context={'project':project,'programs':programs})

    bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})


def list_all_projects_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_all_projects_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_all_projects_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]


    country = Country.objects.get(name=country_name.replace('_',' '))
    projects = Project.objects.filter(country__name=country_name.replace('_',' '))

    from django.core.paginator import Paginator
    projects = Paginator(projects,10)

    callback_data = '/list_all_projects_in_'+country_name
    reply_markup=[[]]
    projects = projects.page(page)

    if projects.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(projects.previous_page_number())})

    if projects.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(projects.next_page_number())})

    projects = projects.object_list

    output = render_to_string('projects_list.html',context={'projects':projects,'title':'Projects in','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})


def list_active_projects_in_country(bot,update):
    if update.message:
        country_name = update.message.text.split("/list_active_projects_in_")[1]
        page = 1
    else:
        country_name = update.callback_query.data.split("/list_active_projects_in_")[1]
        # set page
        page = country_name.split('_')
        country_name = page[:len(page)-1]
        country_name = ' '.join(country_name)
        page = page[len(page)-1]


    import datetime
    today = datetime.date.today()

    country = Country.objects.get(name=country_name.replace('_',' '))
    projects = Project.objects.filter(country__name=country_name.replace('_',' '),end_date__gte=today)

    from django.core.paginator import Paginator
    projects = Paginator(projects,10)

    callback_data = '/list_active_projects_in_'+country_name
    reply_markup=[[]]
    projects = projects.page(page)

    if projects.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(projects.previous_page_number())})

    if projects.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(projects.next_page_number())})

    projects = projects.object_list

    output = render_to_string('projects_list.html',context={'projects':projects,'title':'Active projects in','country_name':country_name,'country_id':country.id})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})


def list_all_uniterra_projects(bot,update):
    from django.db.models import Q
    projects = Project.objects.filter(Q(name__icontains='uniterra') | Q(doner__icontains="uniterra"))
    page = 1

    from django.core.paginator import Paginator
    projects = Paginator(projects,10)

    callback_data = '/uniterra'
    reply_markup=[[]]
    projects = projects.page(page)

    if projects.has_previous():
        reply_markup[0].append({'text':'Previous page','callback_data':callback_data+'_'+str(projects.previous_page_number())})

    if projects.has_next():
        reply_markup[0].append({'text':'Next page','callback_data':callback_data+'_'+str(projects.next_page_number())})

    projects = projects.object_list

    output = render_to_string('projects_list.html',context={'projects':projects,'title':'Projects from','country_name':"Uniterra",'country_id':1})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML',reply_markup={"inline_keyboard":reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':reply_markup})

def subscribe_to_project(bot, update):
    project_id = update.callback_query.data.split('/subscribe_project_')[1]

    if update.callback_query.from_user.username:
        username = update.callback_query.from_user.username
    elif update.callback_query.from_user.last_name:
        username = update.callback_query.from_user.first_name+' '+update.callback_query.from_user.last_name
    else:
        username = update.callback_query.from_user.first_name
        
    chat_id = update.callback_query.message.chat.id
    programs = Program.objects.filter(project__id=project_id)
    subscription = ProgramSubscription.objects.filter(chat_id=chat_id)
    if subscription:
        subscription = subscription[0]
        subscribed_programs = subscription.programs.values_list('id',flat=True)
        for p in programs:
            if not p.pk in subscribed_programs:
                subscription.programs.add(str(p.pk))

        subscription.username = username
    else:
        programs = [str(p.pk) for p in programs]
        subscription = ProgramSubscription(username=username,chat_id=chat_id)
        subscription.save()
        subscription.programs.add(*programs)

    subscription.save()

    bot.answer_callback_query(update.callback_query.id, text='Subscribed! you will recieve notifications.')
