from log_app.models import *
from django.template.loader import render_to_string

comment_states = {}
def comment_instruction(bot,update):
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    comment_states[update.callback_query.from_user.id]=update.callback_query.data
    bot.answerCallbackQuery(update.callback_query.id,text="Send your comment via the chat input field.")

    return 0


def add_comment(bot,update):
    # /add_comment__3247
    print comment_states
    if update.message.from_user.id in comment_states:
        log_id = comment_states[update.message.from_user.id].split('/add_comment__')[1]
         


    bot.sendMessage(update.message.chat.id, text='Your comment is noted. Thank You!')
