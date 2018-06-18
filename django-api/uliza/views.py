from django.shortcuts import render

# Create your views here.



def webhook(request):
                data = json.loads(request.body)
                print ('testing')
                #process_webhook(data)
                return HttpResponse(status=200)
