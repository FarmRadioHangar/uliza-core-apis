from rest_framework import generics
from uliza.models import Participant, ParticipantRegistrationStatusLog
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

    def perform_update(self, serializer):
        pk = self.kwargs['id']
        if pk and 'PATCH' == self.request.method and 'registration_status' in self.request.data:
            participant = Participant.objects.all().get(pk=pk)
            if 'NOT_REGISTERED' == participant.registration_status:
                event_type = self.event_type(serializer.validated_data['registration_status'])
                log_entry = ParticipantRegistrationStatusLog(
                        registration_call=participant.registration_call,
                        participant=participant, 
                        event_type=event_type)
                log_entry.save()
        serializer.save()

    def event_type(self, status):
        if status == 'REGISTERED':
            return 'REGISTRATION_COMPLETE'
        elif status == 'DECLINED':
            return 'REGISTRATION_DECLINED'
        return None
