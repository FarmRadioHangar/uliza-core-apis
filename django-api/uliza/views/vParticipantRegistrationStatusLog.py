from rest_framework import status
from uliza.models import ParticipantRegistrationStatusLog
from uliza.serializers import ParticipantRegistrationStatusLogSerializer
from django.http import HttpResponse, JsonResponse

def default(request):

    if request.method == 'GET': 

        logEntries = ParticipantRegistrationStatusLog.objects.all()
        serializer = ParticipantRegistrationStatusLogSerializer(logEntries, many=True)

        return JsonResponse(serializer.data, safe=False)
        
    elif request.method == 'POST': 

        serializer = ParticipantRegistrationStatusLogSerializer(data=request.data)

        if serializer.is_valid():

            serializer.save()

            return JsonResponse(serializer.data, 
                                status=status.HTTP_201_CREATED, 
                                safe=False)

        return JsonResponse(serializer.errors,
                            status=status.HTTP_400_BAD_REQUEST, 
                            safe=False)

def instance(request, id):

    if request.method == 'GET': pass
        
    elif request.method == 'PUT': pass

    elif request.method == 'PATCH': pass

    return False

