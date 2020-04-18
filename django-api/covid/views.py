from django.shortcuts import render
from covid.models import Content
from django.http import HttpResponse

# Create your views here.
def content(request):
    if 'topic' in request.GET:
        content = Content.objects.filter(topic_en=request.GET['topic'])
        lang = 'en'
    elif 'topic_fr' in request.GET:
        content = Content.objects.filter(topic_fr=request.GET['topic_fr'])
        lang = 'fr'
    elif 'topic_am' in request.GET:
        content = Content.objects.filter(topic_am=request.GET['topic_am'])
        lang = 'am'
    else:
        HttpResponse('Content error')

    if len(content)==0:
        return HttpResponse('Content error')

    content = content[0]
    topic = getattr(content,'topic_'+lang)
    content = getattr(content,'content_'+lang)

    return render(request, 'bot_content.html', {'topic':topic,'content':content,'lang':lang})
