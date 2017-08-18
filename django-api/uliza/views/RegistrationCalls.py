from rest_framework import generics
from uliza.models import Participant, RegistrationCall, ParticipantRegistrationStatusLog
from uliza.serializers import RegistrationCallSerializer

class RegistrationCalls(generics.ListCreateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer

    def perform_create(self, serializer):
        call = serializer.save()
        found = Participant.objects.filter(phone_number=call.phone_number)
        if (found):
            participant = found.first()
            log_entry = ParticipantRegistrationStatusLog(
                    registration_call=call,
                    participant=participant, 
                    event_type='REGISTRATION_CALL_SCHEDULED')
            log_entry.save()

class RegistrationCallsInstance(generics.RetrieveUpdateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer
    lookup_field = 'id'
