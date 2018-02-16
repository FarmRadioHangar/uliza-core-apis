from django.db import models
from eav.decorators import register_eav

registration_status = (
    ('NOT_REGISTERED', 'Not registered'),
    ('REGISTERED', 'Registered'),
    ('DECLINED', 'Declined')
)


@register_eav()
class Participant(models.Model):
    """
    A mobile phone subscriber who have participated in a poll; uniquely
    identified by their phone number.
    """
    phone_number = models.CharField(max_length=20)
    registration_status = models.CharField(max_length=100,
                                           choices=registration_status)
    created_at = models.DateTimeField(auto_now_add=True)
    location = models.CharField(max_length=100, null=True)

    def attributes_eav_dict(self):
        return self.eav.get_values_dict()

    class Meta:
        db_table = 'uliza_participants'


call_status = (
    ('SCHEDULED', 'Scheduled'),
    ('COMPLETE', 'Complete')
)


class RegistrationCall(models.Model):
    """
    A scheduled VOTO call associated with a registration tree.
    """
    schedule_time = models.DateTimeField(null=True)
    voto_call_id = models.IntegerField(unique=True, null=True)
    voto_tree_id = models.IntegerField()
    voto_survey_call_id = models.IntegerField()
    participant = models.ForeignKey(Participant)
    phone_number = models.CharField(max_length=20)
    interactions = models.TextField(null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    call_status = models.CharField(max_length=100,
                                   choices=call_status)

    class Meta:
        db_table = 'uliza_registration_calls'


registration_event_types = (
    ('REGISTRATION_CALL_SCHEDULED', 'A registration call was scheduled'),
    ('REGISTRATION_DECLINED', 'Registration declined'),
    ('REGISTRATION_COMPLETE', 'Registration complete')
)


class ParticipantRegistrationStatusEvent(models.Model):
    """
    Registration status change log for :model:`Participant`s.
    """
    participant = models.ForeignKey('Participant')
    event_type = models.CharField(max_length=100,
                                  choices=registration_event_types)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        db_table = 'uliza_participant_registration_status_events'
