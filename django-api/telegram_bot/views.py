from django.shortcuts import render
from django.http import HttpResponse,HttpResponseForbidden

def start(request):
    return HttpResponse('This is FRI speaking... what can I help you?')
