from django.db import models
from eav.decorators import register_eav

registration_status = (
    ('NOT_REGISTERED', 'Not registered'),
    ('REGISTERED', 'Registered'),
    ('DECLINED', 'Declined')
)


@register_eav()
class Participant(models.Model):
    phone_number = models.CharField(max_length=20)
    registration_status = models.CharField(max_length=100,
                                           choices=registration_status)
    registration_call = models.ForeignKey('RegistrationCall', null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    location = models.CharField(max_length=20,
                                null=True)

    def attributes_eav_dict(self):
        return self.eav.get_values_dict()

    class Meta:
        db_table = 'uliza_participants'


class RegistrationCall(models.Model):
    phone_number = models.CharField(max_length=20)
    scheduled_time = models.DateTimeField()
    voto_id = models.IntegerField()
    voto_tree_id = models.IntegerField()
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'uliza_registration_calls'


registration_event_types = (
    ('REGISTRATION_CALL_SCHEDULED', 'A registration call was scheduled'),
    ('REGISTRATION_DECLINED', 'Registration declined'),
    ('REGISTRATION_COMPLETE', 'Registration complete')
)


class ParticipantRegistrationStatusLog(models.Model):
    participant = models.ForeignKey('Participant')
    registration_call = models.ForeignKey('RegistrationCall', null=True)
    event_type = models.CharField(max_length=100,
                                  choices=registration_event_types)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'uliza_participant_registration_status_log'


class VotoWebhookLog(models.Model):
    endpoint = models.CharField(max_length=100)
    data = models.TextField(null=True, blank=True)
    log_time = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'uliza_voto_webhook_log'


class VotoSurveyRegistrationTree(models.Model):
    voto_survey_id = models.IntegerField(null=True, unique=True)
    voto_tree_id = models.IntegerField(null=True)

    class Meta:
        db_table = 'uliza_voto_survey_registration_tree'
