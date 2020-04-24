# -*- coding: utf-8 -*-
from api_core.settings import MEDIA_URL,MEDIA_ROOT
from covid.models import ChatUser, Question

def covid_start(bot,update):
    bot.sendMessage(update.message.chat_id, text='👋😷\n\n This is a Telegram bot from Farm Radio International.\n\nHere you can find information and resources for broadcasters on Coronavirus (COVID-19). ')
    bot.sendMessage(update.message.chat_id, text='What do you want to do?',reply_markup=
                    {'inline_keyboard':[[{'text':'🦠Learn about COVID-19','callback_data':'/learn'}],
                                        [{'text':'🎙Get tips and resources','callback_data':'/tips_and_resources'}],
                                        [{'text':'❓Ask question or comment','callback_data':'/ask'}]
                                        ]})

def learn(bot,update):
    reply_markup=[[{'text':'How the virus spread','callback_data':'/how_virus_is_spread'}],\
                  [{'text':'Symptoms and infection','callback_data':'/symptoms_of_infection'}],\
                  [{'text':'Myths, misinformation & fake news','callback_data':'/myths_misinformation'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/covid-19-response_blog.jpg',caption="<b>\nSelect the topic you want to learn more about</b>",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

def tips_and_resources(bot,update):
    reply_markup=[[{'text':'Safety for broadcasters','callback_data':'/safety_for_broadcasters'}],\
                  [{'text':'Symptoms and infection','callback_data':'/broadcaster_resources'}],\
                  [{'text':'Myths, misinformation & fake news','callback_data':'/join_online_groups'}]]
    bot.sendPhoto(update.callback_query.message.chat_id,'https://farmradio.org/wp-content/uploads/2020/03/Precious-Naturinda-website.jpg',\
                  caption="<b>Stay safe while still working</b>\n\nSelect the topic you want to learn more about",parse_mode='HTML',reply_markup={'inline_keyboard':reply_markup})

question_states = {}
def question_instruction(bot,update):
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    question_states[update.callback_query.from_user.id]=update.callback_query.data
    bot.sendMessage(update.callback_query.message.chat.id,text="Send your question or comment using the chat text or voice inputs.")

    return 1


def get_question(bot,update):
    chat_user = ChatUser.objects.filter(user_id='t-'+str(update.message.from_user.id))

    if update.message.from_user.id in question_states:
        if not chat_user:
            if update.message.from_user.username and update.message.from_user.first_name and update.message.from_user.last_name:
                telegram_username = update.message.from_user.first_name+' '+update.message.from_user.last_name+' (@'+update.message.from_user.username+')'
            elif update.message.from_user.username and update.message.from_user.first_name:
                telegram_username = update.message.from_user.first_name +' (@'+update.message.from_user.username+')'
            elif update.message.from_user.username:
                telegram_username = update.message.from_user.username
            else:
                telegram_username = update.message.from_user.first_name

            chat_user = ChatUser.objects.create(full_name=telegram_username,user_id='t-'+str(update.message.from_user.id))
        else:
            chat_user = chat_user[0]

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
            return -1

        Question.objects.create(chat_user=chat_user,type=type,content=content)


    if not chat_user.radio_station or not chat_user.country:
        bot.sendMessage(update.message.chat.id, text='Which Radio station do you work for?')
        return 2
    else:
        bot.sendMessage(update.message.chat.id, text='Your question is recieved. Thank You!')
        return -1

def get_country(bot,update):
    code = update.callback_query.data.split('_')[1]
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    chat_user = ChatUser.objects.get(user_id='t-'+str(update.callback_query.from_user.id))
    chat_user.country = code
    chat_user.save()

    bot.sendMessage(update.callback_query.message.chat.id, text='Your question/comment is recieved. Thank You!')
    return -1

def get_radio_station(bot,update):
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    chat_user = ChatUser.objects.get(user_id='t-'+str(update.message.from_user.id))
    chat_user.radio_station = update.message.text
    chat_user.save()

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
    return 3
