from rest_framework import serializers
from uliza.models import (Participant,
                          RegistrationCall,
                          ParticipantRegistrationStatusLog,
                          VotoWebhookLog,
                          VotoSurveyRegistrationTree,
                          registration_status)
from eav.models import Attribute


class ParticipantSerializer(serializers.Serializer):

    id = serializers.IntegerField(read_only=True)
    phone_number = serializers.CharField(max_length=100)
    registration_status = serializers.ChoiceField(choices=registration_status)
    registration_call = serializers.PrimaryKeyRelatedField(read_only=True)
    created_at = serializers.DateTimeField(required=False)
    attributes = serializers.DictField(
            required=False,
            child=serializers.CharField(),
            source='attributes_eav_dict'
    )

    def create(self, validated_data):
        instance = Participant(
                phone_number=validated_data.get('phone_number'),
                registration_status=validated_data.get('registration_status')
        )
        instance.save()
        return instance

    def update(self, instance, validated_data):
        instance.phone_number = validated_data.get(
                'phone_number', instance.phone_number)
        instance.registration_status = validated_data.get(
                'registration_status', instance.registration_status)
        instance.created_at = validated_data.get(
                'created_at', instance.created_at)

        attrs = validated_data.get('attributes_eav_dict')
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


class VotoSurveyRegistrationTreeSerializer(serializers.ModelSerializer):

    class Meta:
        model = VotoSurveyRegistrationTree
        fields = '__all__'
