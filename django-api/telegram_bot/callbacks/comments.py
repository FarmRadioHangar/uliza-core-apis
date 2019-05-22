from log_app.models import *
from django.template.loader import render_to_string

comment_states = {}
def comment_instruction(bot,update):
    # answerCallbackQuery(callback_query_id, text=None, show_alert=False, url=None, cache_time=None, timeout=None, **kwargs)
    comment_states[update.callback_query.from_user.id]=update.callback_query.data
    bot.sendMessage(update.callback_query.message.chat.id,text="Send your comment via the chat input field.")

    return 1


def add_comment(bot,update):
    # /add_comment__3247
    if update.message.from_user.id in comment_states:
        log_id = comment_states[update.message.from_user.id].split('/add_comment__')[1]
        log = Log.objects.get(pk=log_id)
        telegram_username = update.message.from_user.first_name+' '+update.message.from_user.last_name+' (@'+update.message.from_user.username+')'
        Comment.objects.create(log=log,content=update.message.text,telegram_username=telegram_username)

    bot.sendMessage(update.message.chat.id, text='Your comment is noted. Thank You!')

def show_comments(bot,update):
    if update.message:
        id = update.message.text.split("/show_comments__")[1]
        username = update.message.from_user.username
    else:
        id = update.callback_query.data.split('/show_comments__')[1]
        username = update.callback_query.from_user.username

    comments = Comment.objects.filter(log__id=id,telegram_username__icontains=username)
    output = render_to_string('comments_list.html',context={'comments':comments})

    if update.message:
        bot.sendMessage(update.message.chat_id,text=output,parse_mode='HTML')
    else:
        bot.sendMessage(update.callback_query.message.chat_id,text=output,parse_mode='HTML')

def delete_comment(bot,update):
    comment = update.message.text.split('/delete_comment__')[1]
    comment = Comment.objects.get(id = comment)

    comment.delete()
    bot.sendMessage(update.message.chat.id, text='Comment deleted.')
