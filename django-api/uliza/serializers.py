from rest_framework import serializers
from uliza.models import (Participant,
                          RegistrationCall,
                          registration_status)
from eav.models import Attribute
from django.core.exceptions import ValidationError
import phonenumbers


class PhoneNumberValidator:

    def validate_phone_number(self, value):
        if len(value) and value[0] != '+':
            value = '+' + value
        try:
            number = phonenumbers.parse(value)
        except phonenumbers.NumberParseException:
            raise ValidationError('Bad phone number')
        return phonenumbers.format_number(
                number,
                phonenumbers.PhoneNumberFormat.E164
        )


class ParticipantSerializer(serializers.Serializer, PhoneNumberValidator):

    id = serializers.IntegerField(read_only=True)
    phone_number = serializers.CharField(max_length=100)
    registration_status = serializers.ChoiceField(choices=registration_status)
    created_at = serializers.DateTimeField(required=False)
    location = serializers.CharField(max_length=100, required=False)
    attributes = serializers.DictField(
            required=False,
            child=serializers.CharField(),
            source='attributes_eav_dict'
    )

    def create(self, validated_data):
        instance = Participant(
                phone_number=validated_data.get('phone_number'),
                registration_status=validated_data.get('registration_status'),
                location=validated_data.get('location')
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
        instance.location = validated_data.get('location', instance.location)

        attrs = validated_data.get('attributes_eav_dict')
        if attrs is not None:
            for i, (key, value) in enumerate(attrs.iteritems()):
                if not Attribute.objects.filter(name=key).exists():
                    Attribute.objects.create(name=key,
                                             datatype=Attribute.TYPE_TEXT)
                instance.eav.__setattr__(key, value)
        instance.save()
        return instance


class RegistrationCallSerializer(
        serializers.ModelSerializer,
        PhoneNumberValidator):

    class Meta:
        model = RegistrationCall
        fields = '__all__'
