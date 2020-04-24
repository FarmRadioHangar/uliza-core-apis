# -*- coding: utf-8 -*-
from api_core.settings import MEDIA_URL,MEDIA_ROOT
from covid.models import ChatUser, Question
from django.template.loader import render_to_string

covid_reply_markup=[[{'text':'Learn more about COVID-19','callback_data':'/learn'},\
                  {'text':'Go to start','callback_data':'/start'}],[{'text':'Do you have a question?','callback_data':'/ask'}]]

def covid_start(bot,update):
    if update.message:
        chat_id = update.message.chat_id
        bot.sendMessage(chat_id, text='👋😷\n\nHi!')
    else:
        chat_id = update.callback_query.message.chat_id

    bot.sendMessage(chat_id, text='This is a Telegram bot from Farm Radio International.\n\nHere you can find information and resources for broadcasters on Coronavirus (COVID-19).\n\n What do you want to do?',reply_markup=
                    {'inline_keyboard':[[{'text':'🦠Learn about COVID-19','callback_data':'/learn'}],
                                        [{'text':'🎙Tips and resources for broadcasters','callback_data':'/tips_and_resources'}],
                                        [{'text':'❓Ask question or comment','callback_data':'/ask'}]
                                        ]})

def learn(bot,update):
    reply_markup=[[{'text':'How the virus spread','callback_data':'/how_the_virus_is_spread'}],\
                  [{'text':'Precautionary measures','callback_data':'/precautionary_measures'}],\
                  [{'text':'Symptoms and infection','callback_data':'/symptoms_of_infection'}],\
                  [{'text':'Myths, misinformation & fake news I','callback_data':'/myths_misinformation_1'}],\
                  [{'text':'Myths, misinformation & fake news II','callback_data':'/myths_misinformation_2'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/covid-19-response_blog.jpg',caption="<b>\nSelect the topic you want to learn more about</b>",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def tips_and_resources(bot,update):
    reply_markup=[[{'text':'Safety for broadcasters','callback_data':'/safety_for_broadcasters'}],\
                  [{'text':'Broadcaster resources','callback_data':'/broadcaster_resources'}],\
                  [{'text':'Join online groups','callback_data':'/join_online_groups'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/Precious-Naturinda-website.jpg',\
                  caption="<b>Stay safe while still working</b>\n\nSelect the topic you want to learn more about",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def how_virus_is_spread(bot,update):
    from covid.models import Content
    content = Content.objects.filter(title='how_the_virus_is_spread')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = 'en'
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

def precautionary_measures(bot,update):
    from covid.models import Content
    content = Content.objects.filter(title='how_the_virus_is_spread')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = 'en'
    topic = content[1]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

def symptoms_of_infection(bot,update):
    from covid.models import Content
    content = Content.objects.filter(title='symptoms_of_infection')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = 'en'
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

def safety_for_broadcasters(bot,update):
    from covid.models import Content
    content = Content.objects.filter(title='safety_for_broadcasters')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = 'en'
    topic = content[0]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})
    reply_markup = [[{'text':'See other tips for broadcasters','callback_data':'/tips_and_resources'},\
                   {'text':'Go to start','callback_data':'/start'}],[{'text':'Do you have a question?','callback_data':'/ask'}]]
    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def myths_misinformation(bot,update):
    from covid.models import Content
    index = update.callback_query.data.split('_')[2]
    content = Content.objects.filter(title='myths_misinformation')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    topics = []
    lang = 'en'
    topic = content[int(index)-1]
    topics.append({'topic':getattr(topic,'topic_'+lang),'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':covid_reply_markup})

def join_online_groups(bot,update):
    reply_markup = [[{'text':'Tips for broadcasters','callback_data':'/tips_and_resources'},\
                   {'text':'Go to start','callback_data':'/start'}]]
    bot.sendMessage(update.callback_query.message.chat.id,text='<b>🎙 Join online broadcaster groups</b> \n\nLinks to online groups will be shared here soon.\n --',
                    parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def broadcaster_resources(bot,update):
    reply_markup = [[{'text':'Tips for broadcasters','callback_data':'/tips_and_resources'},\
                   {'text':'Go to start','callback_data':'/start'}]]
    bot.sendMessage(update.callback_query.message.chat.id,text='<b>🎙 Broadcaster resources</b> \n\nLinks to online resources will be shared here soon. \n --',
                    parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

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

def update_user_state(id,state=None):
    chat_user = ChatUser.objects.filter(user_id='t-'+str(id))

    if chat_user:
        if state:
            chat_user[0].state = state
            chat_user[0].save()
        return chat_user[0]
    else:
        return False

INSTRUCTION, QUESTION, RADIOSTATION, COUNTRY = range(4)

def question_instruction(bot,update):
    if update.callback_query:
        from_user =  update.callback_query.from_user
        chat_user = update_user_state(from_user.id,QUESTION)
        message = update.callback_query.message
    elif update.message:
        from_user =  update.message.from_user
        message = update.message
        chat_user = update_user_state(update.message.from_user.id,QUESTION)

    if not chat_user:
        chat_user = ChatUser.objects.create(full_name=get_username(from_user),user_id='t-'+str(from_user.id))

    bot.sendMessage(message.chat.id,text="❓What is your question or comment? Use the chat text or voice inputs.")

def conversational_dispatch(bot,update):
    chat_user = update_user_state(update.message.from_user.id)
    if chat_user.state == QUESTION:
        get_question(bot,update)
    elif chat_user.state == RADIOSTATION:
        get_radio_station(bot,update)
    elif chat_user.state == COUNTRY:
        get_country(bot,update)
    else:
        question_instruction(bot,update)

def get_question(bot,update):
    chat_user = update_user_state(update.message.from_user.id)

    if update.message.voice:
        file = bot.getFile(update.message.voice.file_id)
        file_name = 't_voice_'+str(update.message.voice.file_id)+'.ogg'
        file.download(file_name)
        content = 'https://log.uliza.fm'+MEDIA_URL+file_name
        type = 'audio'
    elif update.message.text:
        type = 'text'
        content = update.message.text
    else:
        bot.sendMessage(update.message.chat.id, text='Content error: Message format not supported')
        chat_user.state = -1
        chat_user.save()

    Question.objects.create(chat_user=chat_user,type=type,content=content)


    if not chat_user.radio_station or not chat_user.country:
        bot.sendMessage(update.message.chat.id, text='Which Radio station do you work for?')
        chat_user.state = RADIOSTATION
        chat_user.save()
    else:
        bot.sendMessage(update.message.chat.id, text='Your question is recieved. You will be notified with the answer soon.\nThank You!')
        chat_user.state = -1
        chat_user.save()

def get_country(bot,update):
    code = update.callback_query.data.split('_')[1]
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    chat_user = ChatUser.objects.get(user_id='t-'+str(update.callback_query.from_user.id))
    chat_user.country = code

    bot.sendMessage(update.callback_query.message.chat.id, text='Your question/comment is recieved. Thank You!')

    chat_user.state = -1
    chat_user.save()

def get_radio_station(bot,update):
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    chat_user = ChatUser.objects.get(user_id='t-'+str(update.message.from_user.id))
    chat_user.radio_station = update.message.text

    bot.sendMessage(update.message.chat.id, text='Where are you from?',reply_markup=
                    {'inline_keyboard':[[{'text':'Ethiopia','callback_data':'/country_et'},
                                         {'text':'Tanzania','callback_data':'/country_tz'}],
                                         [{'text':'Uganda','callback_data':'/country_ug'},
                                         {'text':'Kenya','callback_data':'/country_ke'}],
                                         [{'text':'Ghana','callback_data':'/country_gh'},
                                         {'text':'Burkina Faso','callback_data':'/country_bf'}],
                                         [{'text':'Nigeria','callback_data':'/country_ng'},
                                         {'text':'Senegal','callback_data':'/country_tz'}],
                                         [{'text':'Mali','callback_data':'/country_ml'},
                                         {'text':'Malawi','callback_data':'/country_mw'}],
                                         [{'text':'Other','callback_data':'/country_other'}],
                                        ]})
    chat_user.state = COUNTRY
    chat_user.save()
