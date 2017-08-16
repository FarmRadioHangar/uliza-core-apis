from django.http import JsonResponse
from rest_framework import status
from rest_framework.decorators import api_view
from rest_framework.exceptions import NotFound
from uliza.models import Participant
from uliza.serializers import ParticipantSerializer

@api_view(['POST', 'GET'])
def default(request):

    if request.method == 'GET': 

        participants = Participant.objects.all()
        serializer = ParticipantSerializer(participants, many=True)

        return JsonResponse(serializer.data, safe=False)

    elif request.method == 'POST':

        serializer = ParticipantSerializer(data=request.data)

        if serializer.is_valid():

            serializer.save()

            return JsonResponse(serializer.data, status=status.HTTP_201_CREATED)

        return JsonResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

@api_view(['PUT', 'PATCH', 'GET'])
def instance(request, id):

    try:
        participant = Participant.objects.get(pk=id)
    except Participant.DoesNotExist:
        raise NotFound

    if request.method == 'GET': 
        
        serializer = ParticipantSerializer(participant)

        return JsonResponse(serializer.data)

    elif request.method == 'PUT' or request.method == 'PATCH': 

        if request.method == 'PATCH':
            data = participant.__dict__.copy()
            data.update(request.data)
        else:
            data = request.data

        serializer = ParticipantSerializer(participant, data=data)

        if serializer.is_valid():

            serializer.save()

            return JsonResponse(serializer.data)

        return JsonResponse(serializer.errors, 
                            status=status.HTTP_400_BAD_REQUEST)

    elif request.method == 'DELETE': 
        
        pass 

    return False
