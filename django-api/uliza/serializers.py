from rest_framework import serializers
from uliza.models import (Participant,
                          RegistrationCall,
                          ParticipantRegistrationStatusLog,
                          VotoWebhookLog)
from eav.models import Attribute


class ParticipantSerializer(serializers.Serializer):

    phone_number = serializers.CharField(max_length=100)
    registration_status = serializers.CharField(max_length=100)
    created_at = serializers.DateTimeField()
    attributes = serializers.DictField(
            required=False,
            child=serializers.CharField()
    )

    def create(self, validated_data):
        instance = Participant(**validated_data)
        instance.save()
        return instance

    def update(self, instance, validated_data):
        instance.phone_number = validated_data.get(
                'phone_number', instance.phone_number)
        instance.registration_status = validated_data.get(
                'registration_status', instance.registration_status)
        instance.created_at = validated_data.get(
                'created_at', instance.created_at)

        attrs = validated_data.get('attributes')
        if attrs is not None:
            for i, (key, value) in enumerate(attrs.iteritems()):
                if not Attribute.objects.filter(name=key).exists():
                    Attribute.objects.create(name=key,
                                             datatype=Attribute.TYPE_TEXT)
                instance.eav.__setattr__(key, value)
        instance.save()
        return instance


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
