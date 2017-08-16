from rest_framework import status
from uliza.models import Participant
from uliza.serializers import ParticipantSerializer
from django.http import HttpResponse, JsonResponse

def default(request):

    if request.method == 'GET': 

        participants = Participant.objects.all()
        serializer = ParticipantSerializer(participants, many=True)

        return JsonResponse(serializer.data, safe=False)

    elif request.method == 'POST':

        serializer = ParticipantSerializer(data=request.data)

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

