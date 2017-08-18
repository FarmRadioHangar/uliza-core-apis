from rest_framework import generics
from uliza.models import Participant
from uliza.serializers import ParticipantSerializer

class Participants(generics.ListCreateAPIView):

    queryset = Participant.objects.all()
    model = Participant
    serializer_class = ParticipantSerializer
    filter_fields = ['phone_number']

class ParticipantsInstance(generics.RetrieveUpdateAPIView):

    queryset = Participant.objects.all()
    model = Participant
    serializer_class = ParticipantSerializer
    lookup_field = 'id'
