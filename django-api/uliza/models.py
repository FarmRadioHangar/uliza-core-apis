from django.db import models
from eav.decorators import register_eav

registration_status = (
    ('NOT_REGISTERED', 'Not registered'),
    ('REGISTERED', 'Registered'),
    ('DECLINED', 'Declined')
)

OPEN = 'open'
CLOSED = 'closed'

CALL_STATE = (
	(OPEN, 'open'),
	(CLOSED, 'closed')
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
    registration_call = models.ForeignKey('RegistrationCall', null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    location = models.CharField(max_length=100, null=True)

    def attributes_eav_dict(self):
        return self.eav.get_values_dict()

    class Meta:
        db_table = 'uliza_participants'


class RegistrationCall(models.Model):
    """
    A scheduled VOTO call associated with a registration tree.
    """
    phone_number = models.CharField(max_length=20)
    scheduled_time = models.DateTimeField()
    voto_call_id = models.IntegerField()
    voto_tree_id = models.IntegerField()
    created_at = models.DateTimeField(auto_now_add=True)
    interactions = models.TextField(null=True)

    class Meta:
        db_table = 'uliza_registration_calls'


registration_event_types = (
    ('REGISTRATION_CALL_SCHEDULED', 'A registration call was scheduled'),
    ('REGISTRATION_DECLINED', 'Registration declined'),
    ('REGISTRATION_COMPLETE', 'Registration complete')
)


class ParticipantRegistrationStatusLog(models.Model):
    """
    Registration status change log for :model:`Participant`s.
    """
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

class Answer(models.Model):
	zammad_id = models.IntegerField(null=False, unique=False)
	subscriber_phone = models.CharField(max_length=20)
	audio = models.TextField(null=True, blank=True)
	articles_count = models.PositiveSmallIntegerField(default=1)
	state = models.CharField(max_length=10, choices=CALL_STATE, default=OPEN)
	created_at = models.DateTimeField(auto_now_add=True)

	'''def serialize_hook(self, hook):
		return {
			'hook': hook.dict(),
			'data': {
			    'id': self.id,
			    'zammad_id': self.zammad_id,
			    'subscriber_phone': self.subscriber_phone,
			    'state_id': self.state_id,
		               }
			}

	
	def mark_as_read(self):
		from rest_hooks.signals import hook_event
		hook_event.send(
		    sender=self.__class__,
		    action='read',
		    instance=self
		)
	'''

class Role(models.Model):
	name = models.CharField(max_length=50)
	description = models.CharField(max_length=50)
	
class Detail(models.Model):
	name = models.CharField(max_length=50)

class Contact(models.Model):
	role = models.ForeignKey(Role, related_name='roles', related_query_name='role')

class ContactDetail(models.Model):
	contact = models.ForeignKey(Contact, related_name='+') 
	detail = models.ForeignKey(Detail, related_name='+')
	info = models.CharField(max_length=50)
	meta = models.CharField(max_length=50)

class User(models.Model):
	auth_user_id = models.IntegerField(null=False, unique=False)
	zammad_token = models.CharField(max_length=50)
	firebase_login = models.CharField(max_length=50)
	sip_username = models.CharField(max_length=50)
	sip_password = models.CharField(max_length=20)
	sip_host = models.TextField(null=False, blank=False)
	create_at = models.DateTimeField(auto_now_add=True)
