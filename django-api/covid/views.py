from django.shortcuts import render
from covid.models import Content
from django.http import HttpResponse

# Create your views here.
def content(request,topic,lang):
    if lang == 'en':
        content = Content.objects.filter(topic_en=topic)
    elif lang == 'fr':
        content = Content.objects.filter(topic_fr=topic)
    elif lang == 'am':
        content = Content.objects.filter(topic_am=topic)
    else:
        HttpResponse('Content error')

    if len(content)==0:
        return HttpResponse('Content error')

    content = content[0]
    topic = getattr(content,'topic_'+lang)
    content = getattr(content,'content_'+lang)

    return render(request, 'bot_content.html', {'topic':topic,'content':content,'lang':lang})
