from rest_framework import serializers
from uliza.models import *

class ParticipantSerializer(serializers.ModelSerializer):

    class Meta:
        model = Participant
        fields = '__all__'

class RegistrationCallSerializer(serializers.ModelSerializer):

    class Meta:
        model = RegistrationCall
        fields = '__all__'

class ParticipantRegistrationStatusLogSerializer(serializers.ModelSerializer):

    class Meta:
        model = ParticipantRegistrationStatusLog
        fields = '__all__'

class VotoWebhookLogSerializer(serializers.ModelSerializer):

    class Meta:
        model = VotoWebhookLog
        fields = '__all__'
        
