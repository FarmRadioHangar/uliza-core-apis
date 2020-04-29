# -*- coding: utf-8 -*-
from api_core.settings import MEDIA_URL,MEDIA_ROOT,TELEGRAM_WEBHOOK_SITE
from covid.models import ChatUser, Question
from django.template.loader import render_to_string
from django.utils import translation
from django.utils.translation import gettext as _

def get_username(from_user):
    if from_user.username and from_user.first_name and from_user.last_name:
        telegram_username = from_user.first_name+' '+from_user.last_name+' (@'+from_user.username+')'
    elif from_user.username and from_user.first_name:
        telegram_username = from_user.first_name +' (@'+from_user.username+')'
    elif from_user.username:
        telegram_username = from_user.username
    else:
        telegram_username = from_user.first_name

    return telegram_username

def get_language(bot,update,*chat_user):
    if update.message:
        chat_id = update.message.chat_id
    else:
        chat_id = update.callback_query.message.chat_id

    bot.sendMessage(chat_id, text=_('Language | ቋንቋ | Langue'),reply_markup=
                    {'inline_keyboard':[[{'text':'English','callback_data':'/language_en'},
                                         {'text':'Français','callback_data':'/language_fr'},
                                         {'text':'አማርኛ','callback_data':'/language_am'}],
                                        ]})

def get_user(func):
    def handler(bot,update):
        if update.callback_query:
            from_user = update.callback_query.from_user
        elif update.message:
            from_user = update.message.from_user

        chat_user = ChatUser.objects.filter(user_id='t-'+str(from_user.id))
        if not chat_user:
            chat_user = ChatUser.objects.create(full_name=get_username(from_user),user_id='t-'+str(from_user.id))
            return get_language(bot,update,chat_user[0])
        elif chat_user[0].language:
            translation.activate(chat_user[0].language)
        else:
            return get_language(bot,update,chat_user[0])

        return func(bot,update,chat_user[0])

    return handler

def set_language(bot,update,*chat_user):
    from_user = update.callback_query.from_user
    chat_user = ChatUser.objects.filter(user_id='t-'+str(from_user.id))
    lang = update.callback_query.data.split('_')[1]
    chat_user = chat_user[0]
    chat_user.language = lang
    chat_user.save()
    update.message = update.callback_query.message
    covid_start(bot,update)


@get_user
def covid_start(bot,update,*chat_user):
    if update.message:
        chat_id = update.message.chat_id
        bot.sendMessage(chat_id, text='👋😷\n\n'+_('Hi!'))
    else:
        chat_id = update.callback_query.message.chat_id

    bot.sendMessage(chat_id, text=_('This is a Telegram bot from Farm Radio International(farmradio.org).\n\nHere you can find information and resources for broadcasters on Coronavirus (COVID-19).\n\n What do you want to do?'),reply_markup=
                    {'inline_keyboard':[[{'text':_('%(icon)sLearn about COVID-19')%{'icon':'🦠'},'callback_data':'/learn'}],
                                        [{'text':_('%(icon)sTips and resources for broadcasters')%{'icon':'🎙'},'callback_data':'/tips_and_resources'}],
                                        [{'text':_('%(icon)sAsk question or comment')%{'icon':'❓'},'callback_data':'/ask_confirmation'}]
                                        ]})

@get_user
def learn(bot,update,*chat_user):
    reply_markup=[[{'text':_('How the virus spread'),'callback_data':'/how_the_virus_is_spread'}],\
                  [{'text':_('Precautionary measures'),'callback_data':'/precautionary_measures'}],\
                  [{'text':_('Symptoms and infection'),'callback_data':'/symptoms_of_infection'}],\
                  [{'text':_('Myths, misinformation & fake news I'),'callback_data':'/myths_misinformation_1'}],\
                  [{'text':_('Myths, misinformation & fake news II'),'callback_data':'/myths_misinformation_2'}]]
    if not chat_user[0].language == 'fr':
        image = 'https://farmradio.org/wp-content/uploads/2020/03/covid-19-response_blog.jpg'
    else:
        image = 'https://farmradio.org/wp-content/uploads/2020/03/covid-19-respons-banner_blog-fr.jpg'

    bot.sendPhoto(update.callback_query.message.chat_id,image,caption="<b>\n"+_("Select the topic you want to learn more about")+"</b>",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

@get_user
def tips_and_resources(bot,update,*chat_user):
    reply_markup=[[{'text':_('Safety for broadcasters'),'callback_data':'/safety_for_broadcasters'}],\
                  [{'text':_('Broadcaster resources'),'callback_data':'/broadcaster_resources'}],\
                  [{'text':_('Join online groups'),'callback_data':'/join_online_groups'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/Precious-Naturinda-website.jpg',\
                  caption="<b>"+_("Stay safe while still working")+"</b>\n\n"+_("Select the topic you want to learn more about"),parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

@get_user
def how_virus_is_spread(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='how_the_virus_is_spread')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    covid_reply_markup=[[{'text':_('Learn more about COVID-19'),'callback_data':'/learn'},\
                     {'text':_('Go to start'),'callback_data':'/start'}],
                     [{'text':_('Do you have a question?'),'callback_data':'/ask_confirmation'}]]

    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

@get_user
def precautionary_measures(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='how_the_virus_is_spread')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    covid_reply_markup=[[{'text':_('Learn more about COVID-19'),'callback_data':'/learn'},\
                     {'text':_('Go to start'),'callback_data':'/start'}],
                     [{'text':_('Do you have a question?'),'callback_data':'/ask_confirmation'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[1]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

@get_user
def symptoms_of_infection(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='symptoms_of_infection')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    covid_reply_markup=[[{'text':_('Learn more about COVID-19'),'callback_data':'/learn'},\
                     {'text':_('Go to start'),'callback_data':'/start'}],
                     [{'text':_('Do you have a question?'),'callback_data':'/ask_confirmation'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

@get_user
def safety_for_broadcasters(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='safety_for_broadcasters')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})
    reply_markup = [[{'text':_('See other tips for broadcasters'),'callback_data':'/tips_and_resources'},\
                   {'text':_('Go to start'),'callback_data':'/start'}],[{'text':_('Do you have a question?'),'callback_data':'/ask_confirmation'}]]
    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

@get_user
def myths_misinformation(bot,update,*chat_user):
    from covid.models import Content
    index = update.callback_query.data.split('_')[2]
    content = Content.objects.filter(title='myths_misinformation')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    covid_reply_markup=[[{'text':_('Learn more about COVID-19'),'callback_data':'/learn'},\
                     {'text':_('Go to start'),'callback_data':'/start'}],
                     [{'text':_('Do you have a question?'),'callback_data':'/ask_confirmation'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[int(index)-1]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

@get_user
def join_online_groups(bot,update,*chat_user):
    reply_markup = [[{'text':_('Tips for broadcasters'),'callback_data':'/tips_and_resources'},\
                   {'text':_('Go to start'),'callback_data':'/start'}]]
    bot.sendMessage(update.callback_query.message.chat.id,text='<b>🎙 '+_('Join online broadcaster groups')+'</b> \n\n'+_('Links to online groups will be shared here soon.')+'\n --',
                    parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

@get_user
def broadcaster_resources(bot,update,*chat_user):
    reply_markup = [[{'text':_('Tips for broadcasters'),'callback_data':'/tips_and_resources'},\
                   {'text':_('Go to start'),'callback_data':'/start'}]]
    bot.sendMessage(update.callback_query.message.chat.id,text='<b>🎙 '+_('Broadcaster resources')+'</b> \n\n'+_('Links to online resources will be shared here soon.')+' \n --',
                    parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})


def update_user_state(chat_user,state=None):
    if state:
        chat_user.state = state
        chat_user.save()

    return chat_user

INSTRUCTION, QUESTION, RADIOSTATION, COUNTRY = range(4)
@get_user
def conversational_dispatch(bot,update,*chat_user):
    chat_user = chat_user[0]
    if chat_user.state == QUESTION:
        get_question(bot,update)
    elif chat_user.state == RADIOSTATION:
        get_radio_station(bot,update)
    elif chat_user.state == COUNTRY:
        get_country(bot,update)
    else:
        get_confirmation(bot,update)

# step 1 CONFIRMATION
@get_user
def get_confirmation(bot,update,*chat_user):
    if update.callback_query:
        message = update.callback_query.message
        if update.callback_query.data == '/no':
            bot.sendMessage(message.chat.id,text=_("Ok, no problem."))
            return -1
    else:
        message = update.message

    reply_markup=[[{'text':_('Yes'),'callback_data':'/question_instruction'},\
                  {'text':_('No'),'callback_data':'/no'}]]

    bot.sendMessage(message.chat.id,text="❓"+_("Do you have a question or comment?"),
                    parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

# step 2 INSTRUCTION
@get_user
def question_instruction(bot,update,*chat_user):
    chat_user = chat_user[0]
    if update.callback_query:
        chat_user = update_user_state(chat_user,QUESTION)
        message = update.callback_query.message
    elif update.message:
        message = update.message
        chat_user = update_user_state(chat_user,QUESTION)

    bot.sendMessage(message.chat.id,text="❓"+_("What is your question or comment? Use the chat text or voice inputs."))

# step 3 QUESTION
@get_user
def get_question(bot,update,*chat_user):
    chat_user = chat_user[0]

    if update.message.voice:
        file = bot.getFile(update.message.voice.file_id)
        file_name = 't_voice_'+str(update.message.voice.file_id)+'.ogg'
        file.download(custom_path=MEDIA_ROOT+'/'+file_name)
        content = TELEGRAM_WEBHOOK_SITE+MEDIA_URL+file_name
        type = 'audio'
    elif update.message.text:
        type = 'text'
        content = update.message.text
    else:
        bot.sendMessage(update.message.chat.id, text=_('Content error: Message format not supported'))
        chat_user.state = -1
        chat_user.save()

    Question.objects.create(chat_user=chat_user,type=type,content=content)

    if not chat_user.radio_station or not chat_user.country:
        bot.sendMessage(update.message.chat.id, text=_('What is the name of the Radio station you work for?'))
        chat_user.state = RADIOSTATION
        chat_user.save()
    else:
        bot.sendMessage(update.message.chat.id, text=_('Your question is recieved. You will be notified with the answer soon.\n Thank You!'))
        chat_user.state = -1
        chat_user.save()

# step 4 RADIOSTATION
@get_user
def get_radio_station(bot,update,*chat_user):
    chat_user = chat_user[0]
    chat_user.radio_station = update.message.text

    bot.sendMessage(update.message.chat.id, text=_('Where are you from?'),reply_markup=
                    {'inline_keyboard':[[{'text':'Ethiopia','callback_data':'/country_et'},
                                         {'text':'Tanzania','callback_data':'/country_tz'}],
                                         [{'text':'Uganda','callback_data':'/country_ug'},
                                         {'text':'Kenya','callback_data':'/country_ke'}],
                                         [{'text':'Ghana','callback_data':'/country_gh'},
                                         {'text':'Burkina Faso','callback_data':'/country_bf'}],
                                         [{'text':'Nigeria','callback_data':'/country_ng'},
                                         {'text':'Senegal','callback_data':'/country_sn'}],
                                         [{'text':'Mali','callback_data':'/country_ml'},
                                         {'text':'Malawi','callback_data':'/country_mw'}],
                                         [{'text':'Other','callback_data':'/country_other'}],
                                        ]})
    chat_user.state = COUNTRY
    chat_user.save()

# step 5 COUNTRY
@get_user
def get_country(bot,update,*chat_user):
    code = update.callback_query.data.split('_')[1]
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    chat_user = chat_user[0]
    chat_user.country = code

    bot.sendMessage(update.callback_query.message.chat.id, text=_('Your question/comment is recieved. Thank You!'))

    chat_user.state = -1
    chat_user.save()
