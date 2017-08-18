from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from uliza.models import Participant
from uliza.serializers import ParticipantSerializer

class Participants(generics.ListCreateAPIView):

    queryset = Participant.objects.all()
    model = Participant
    serializer_class = ParticipantSerializer
    filter_fields = ['phone_number']

class ParticipantsEntity(generics.RetrieveUpdateAPIView):

    queryset = Participant.objects.all()
    model = Participant
    serializer_class = ParticipantSerializer
    lookup_field = 'id'

#    def get(self, request, id):
#        pass

#@api_view(['POST', 'GET'])
#def default(request):
#
#    if request.method == 'GET': 
#
#        if 'phone_number' in request.query_params:
#            participants = Participant.objects.(phone_number=request.query_params['phone_number'])
#        else:
#            participants = Participant.objects.all()
#
#        serializer = ParticipantSerializer(participants, many=True)
#
#        return JsonResponse(serializer.data, safe=False)
#
#    elif request.method == 'POST':
#
#        serializer = ParticipantSerializer(data=request.data)
#
#        if serializer.is_valid():
#
#            serializer.save()
#
#            return JsonResponse(serializer.data, status=status.HTTP_201_CREATED)
#
#        return JsonResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
#
#@api_view(['PUT', 'PATCH', 'GET'])
#def instance(request, id):
#
#    try:
#        participant = Participant.objects.get(pk=id)
#    except Participant.DoesNotExist:
#        raise NotFound
#
#    if request.method == 'GET': 
#        
#        serializer = ParticipantSerializer(participant)
#
#        return JsonResponse(serializer.data)
#
#    elif request.method == 'PUT' or request.method == 'PATCH': 
#
#        if request.method == 'PATCH':
#            data = participant.__dict__.copy()
#            data.update(request.data)
#        else:
#            data = request.data
#
#        serializer = ParticipantSerializer(participant, data=data)
#
#        if serializer.is_valid():
#
#            serializer.save()
#
#            return JsonResponse(serializer.data)
#
#        return JsonResponse(serializer.errors, 
#                            status=status.HTTP_400_BAD_REQUEST)
#
#    elif request.method == 'DELETE': 
#        
#        pass 
#
#    return False
