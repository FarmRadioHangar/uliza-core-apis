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
def covid_main_menu(bot,update,*chat_user):
    if update.message:
        chat_id = update.message.chat_id
        bot.sendMessage(chat_id, text='👋😷\n\n'+_('Hello!'))
    else:
        chat_id = update.callback_query.message.chat_id

    bot.sendMessage(chat_id, text='What do you want to do?',reply_markup=
                    {'inline_keyboard':[[{'text':_('%(icon)sLearn about COVID-19')%{'icon':'🦠'},'callback_data':'/learn'}],
                                        [{'text':_('%(icon)sGet radio resources')%{'icon':'🎙'},'callback_data':'/get_radio_resources'}],
                                        [{'text':_('%(icon)sFact-check myths')%{'icon':'❓'},'callback_data':'/fact_check_myths'}]
                                        ]})
@get_user
def covid_start(bot,update,*chat_user):
    if update.message:
        chat_id = update.message.chat_id
        bot.sendMessage(chat_id, text='👋😷\n\n'+_('Hello!'))
    else:
        chat_id = update.callback_query.message.chat_id

    bot.sendMessage(chat_id, text=_('This is the FRI Broadcasters COVID-19 messenger bot.\n\nHere you can find information and resources for broadcasters on coronavirus (COVID-19).\nWe hope this is useful for planning and preparing your radio program.\n\n What do you want to do?'),reply_markup=
                    {'inline_keyboard':[[{'text':_('%(icon)sLearn about COVID-19')%{'icon':'🦠'},'callback_data':'/learn'}],
                                        [{'text':_('%(icon)sGet radio resources')%{'icon':'🎙'},'callback_data':'/get_radio_resources'}],
                                        [{'text':_('%(icon)sFact-check myths')%{'icon':'❓'},'callback_data':'/fact_check_myths'}]
                                        ]})

# COVID-19 content
@get_user
def learn(bot,update,*chat_user):
    reply_markup=[[{'text':_('Basic facts'),'callback_data':'/basic_facts'}],\
                  [{'text':_('How the virus spread'),'callback_data':'/how_the_virus_is_spread'}],\
                  [{'text':_('Preventive measures'),'callback_data':'/preventive_measures'}]]
    if not chat_user[0].language == 'fr':
        image = 'https://farmradio.org/wp-content/uploads/2020/03/covid-19-response_blog.jpg'
    else:
        image = 'https://farmradio.org/wp-content/uploads/2020/03/covid-19-respons-banner_blog-fr.jpg'

    bot.sendPhoto(update.callback_query.message.chat_id,image,caption=_("COVID-19"),parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})


@get_user
def basic_facts(bot,update,*chat_user):
    content = _("The most common symptoms of COVID-19 are fever, tiredness, and dry cough. Symptoms are usually mild and begin gradually. Some infected people have no symptoms and don't feel ill. People with fever, cough, and difficulty breathing should seek medical attention.")
    segment_reply_markup = [[{'text':_('Learn more'),'callback_data':'/more_basic_facts'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def more_basic_facts(bot,update,*chat_user):
    content = _("Most people (about 80%) recover without special treatment. About 1 in 6 people become seriously ill. Older people and people with health issues such as heart problems, diabetes, and high blood pressure are more likely to become seriously ill.")
    segment_reply_markup = [[{'text':_('Important info'),'callback_data':'/important_info'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})


@get_user
def important_info(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='important_info')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    segment_reply_markup = [[{'text':_('How the virus spread?'),'callback_data':'/how_the_virus_is_spread'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def how_the_virus_is_spread(bot,update,*chat_user):
    content = _("People catch COVID-19 from others who have the virus. The disease is spread through small droplets produced when infected people cough, sneeze, or exhale. These droplets can be inhaled by people nearby or land on nearby objects and surfaces. If touched by other people, they can be infected.")
    segment_reply_markup = [[{'text':_('More info'),'callback_data':'/more_how_the_virus_is_spread'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}],
                            [{'text':_('Basic facts'),'callback_data':'/basic_facts'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def more_how_the_virus_is_spread(bot,update,*chat_user):
    content = _("When people inhale droplets or touch contaminated objects or surfaces, then touch their eyes, nose, or mouth, they can be infected. This is why it is important to stay more than 1 metre away from a person who is sick.")
    segment_reply_markup = [[{'text':_('Preventive measures'),'callback_data':'/preventive_measures'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def preventive_measures(bot,update,*chat_user):
    content = _("Wash your hands frequently. Maintain social / physical distancing. Avoid touching your eyes, nose, and mouth. Practice good respiratory hygiene. If you have fever, a cough, and difficulty breathing, seek medical care early. Practice safe greetings. ")
    segment_reply_markup = [[{'text':_('Learn more'),'callback_data':'/more_preventive_measures'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}],
                            [{'text':_('Basic facts'),'callback_data':'basic_facts'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})


@get_user
def more_preventive_measures(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='more_preventive_measures')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    segment_reply_markup = [[{'text':_('Basic facts'),'callback_data':'/basic_facts'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})


# Get radio resources content
@get_user
def get_radio_resources(bot,update,*chat_user):
    reply_markup=[[{'text':_('Working safely'),'callback_data':'/working_safely'}],\
                  [{'text':_('Protect your health'),'callback_data':'/protect_your_health'}],\
                  [{'text':_('Good radio resources'),'callback_data':'/good_radio_resources'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/Precious-Naturinda-website.jpg',\
                  caption="<b>"+_("Broadcaster resources for COVID-19")+"</b>",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})


@get_user
def working_safely(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='working_safely')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    segment_reply_markup = [[{'text':_('Sanitize your equipment'),'callback_data':'/sanitize_your_equipment'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})


@get_user
def sanitize_your_equipment(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='sanitize_your_equipment')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    segment_reply_markup = [[{'text':_('Protect your health'),'callback_data':'/protect_your_health'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})


@get_user
def protect_your_health(bot,update,*chat_user):
    from covid.models import Content
    content = Content.objects.filter(title='protect_your_health')

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    segment_reply_markup = [[{'text':_('Good radio resources'),'callback_data':'/good_radio_resources'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]
    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})
    bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def good_radio_resources(bot,update,*chat_user):
    reply_markup=[[{'text':_('Emergency programs'),'callback_data':'/emergency_programs'}],\
                  [{'text':_('Farm Radio resources'),'callback_data':'/farm_radio_resources'}],\
                  [{'text':_('COVID-19 information'),'callback_data':'/covid_information'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://wire.farmradio.fm/wp-content/uploads/2018/01/broadcaster-resources-image.jpg',\
                  caption="<b>"+_("Good radio resources")+"</b>",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})


@get_user
def emergency_programs(bot,update,*chat_user):
    content = _("Good reporting practices are also important to ensure that people stay calm in a time of emergency and take appropriate action to respond. Farm Radio International has produced a Broadcaster how-to guide on planning and producing effective emergency response programming, and adapted it for the coronavirus pandemic.\n\nhttp://scripts.farmradio.fm/radio-resource-packs/covid-19-resources/planning-producing-effective-emergency-programming-covid/")
    segment_reply_markup = [[{'text':_('Farm radio resources'),'callback_data':'/farm_radio_resources'},\
                             {'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def farm_radio_resources(bot,update,*chat_user):
    content = _("Farm Radio International is producing a variety of information resources to help broadcasters produce good quality radio programming around COVID-19 and the impact of this crisis on rural populations.")
    segment_reply_markup = [[ {'text':_('Farmer stories'),'callback_data':'/farmer_stories'},{'text':_('Key info & radio scripts'),'callback_data':'/key_info_radio_scripts'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def farmer_stories(bot,update,*chat_user):
    content = _("Barza Wire Farmer stories: https://wire.farmradio.fm/tag/emergencies/")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def key_info_radio_scripts(bot,update,*chat_user):
    content = _("Access Farm Radio’s resources on COVID-19:http://scripts.farmradio.fm/radio-resource-packs/covid-19-resources/")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def covid_information(bot,update,*chat_user):
    content = _("COVID-19 information")
    segment_reply_markup = [[{'text':_("FRI's key messenges"),'callback_data':'/fri_key_messenges'}],
                            [{'text':_("WHO resources"),'callback_data':'/who_resources'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def fri_key_messenges(bot,update,*chat_user):
    content = _("Find all these key messages on COVID-19: http://scripts.farmradio.fm/radio-resource-packs/covid-19-resources/key-information-covid-19-broadcasters/")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def who_resources(bot,update,*chat_user):
    content = _("Find all the information and resources from the World Health Organization here: https://www.who.int/emergencies/diseases/novel-coronavirus-2019")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

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
def fact_check_myths(bot,update,*chat_user):
    reply_markup=[[{'text':_('Truth behind myths'),'callback_data':'/truth_behind_myths_init'}],\
                  [{'text':_('Fight fake news'),'callback_data':'/fight_fake_news'}],\
                  [{'text':_('Latest COVID myths'),'callback_data':'/latest_covid_myths'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://wire.farmradio.fm/wp-content/uploads/2020/05/FAQs-COVID-graphic.png',\
                  caption=_("It’s important to dispel myths and fake news so that your audience has the right information to make good decisions about their health, safety, and livelihoods. "),parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

@get_user
def truth_behind_myths(bot,update,*chat_user):
    from covid.models import Content
    page = update.callback_query.data.split('/truth_behind_myths_')[1]

    if page == 'init':
        content  = Content.objects.filter(title='truth_behind_myths_1')
    else:
        content  = Content.objects.filter(title='truth_behind_myths_'+page)

    if len(content) == 0:
        bot.sendMessage(update.callback_query.message.chat.id,'Content error')

    if page == '1' or page == 'init':
        segment_reply_markup = [[{'text':_('Next'),'callback_data':'/truth_behind_myths_2'}]]
    else:
        segment_reply_markup = [[{'text':_('Previous'),'callback_data':'/truth_behind_myths_1'}]]

    segment_reply_markup.append([{'text':_('Go back'),'callback_data':'/go_back'}])

    topics = []
    lang = chat_user[0].language
    topic = content[0]
    topics.append({'topic':"",'content':getattr(topic,'content_'+lang)})

    output = render_to_string('covid_content.html',context={'topics':topics})

    if page == 'init':
        bot.sendMessage(update.callback_query.message.chat.id,text=output,parse_mode='HTML',reply_markup={'inline_keyboard':segment_reply_markup})
    else:
        bot.editMessageText(output,parse_mode='HTML',chat_id=update.callback_query.message.chat.id,message_id=update.callback_query.message.message_id,reply_markup={'inline_keyboard':segment_reply_markup})


@get_user
def fight_fake_news(bot,update,*chat_user):
    content = _("Learn how to spot fake news and fact-check myths in our Broadcaster how-to guide. http://scripts.farmradio.fm/radio-resource-packs/farm-radio-resource-pack-114/bh2-fake-news-identify/")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

@get_user
def latest_covid_myths(bot,update,*chat_user):
    content = _("Africa Check is keeping tabs on the latest myths and misconceptions. Get the latest information: https://africacheck.org/reports/live-guide-all-our-coronavirus-fact-checks-in-one-place/")
    segment_reply_markup = [[{'text':_('Go back'),'callback_data':'/go_back'}]]

    bot.sendMessage(update.callback_query.message.chat.id,text=content,reply_markup={'inline_keyboard':segment_reply_markup})

# question module
# it is disabled (no route is linked to the handlers)
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
