from django.db import models

registration_status = (
    ('NOT_REGISTERED', 'Not registered'),
    ('REGISTERED', 'Registered'),
    ('DECLINED', 'Declined')
)

class Participant(models.Model):
    phone_number = models.CharField(max_length=20)
    registration_status = models.CharField(max_length=20, choices=registration_status)
    registration_call = models.ForeignKey('RegistrationCall', null=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'participants'

class RegistrationCall(models.Model):
    phone_number = models.CharField(max_length=20)
    scheduled_time = models.DateTimeField()
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'registration_calls'

registration_event_types = (
    ('REGISTRATION_CALL_SCHEDULED', 'A registration call was scheduled'),
    ('REGISTRATION_DECLINED', 'Registration declined'),
    ('REGISTRATION_COMPLETE', 'Registration complete')
)

class ParticipantRegistrationStatusLog(models.Model):
    participant = models.ForeignKey('Participant')
    registration_call = models.ForeignKey('RegistrationCall', null=True)
    event_type = models.CharField(max_length=100, choices=registration_event_types)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'participant_registration_status_log'

class VotoResponseData(models.Model):
    data = models.TextField(null=True, blank=True)

    class Meta:
        db_table = 'voto_response_data'
