from django.http import Http404
from rest_framework import generics
from uliza.models import (
        RegistrationCall,
        # ParticipantRegistrationStatusEvent
        )
from uliza.serializers import RegistrationCallSerializer


class RegistrationCalls(generics.ListCreateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer


class RegistrationCallsParticipant(generics.RetrieveUpdateAPIView):

    model = RegistrationCall
    serializer_class = RegistrationCallSerializer

    def get_object(self):
        calls = RegistrationCall.objects.filter(
            participant=self.kwargs.get('participant')
        ).order_by('schedule_time')

        if 0 == len(calls):
            raise Http404

        obj = calls[0]
        self.check_object_permissions(self.request, obj)
        return obj


class RegistrationCallsInstance(generics.RetrieveUpdateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer
    lookup_field = 'id'


class RegistrationCallsVotoInstance(generics.RetrieveUpdateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer
    lookup_field = 'voto_call_id'
