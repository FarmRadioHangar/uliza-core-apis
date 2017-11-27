from django.db import models
from django.contrib.auth.models import User
from api_core.settings import GDRIVE_STORAGE
from django.utils.translation import ugettext, ugettext_lazy as _
from django.core import validators

class Auth0User(models.Model):
    username = models.CharField(_('username'), max_length=30, unique=True,
        help_text=_('Required. 30 characters or fewer. Letters, digits and '
                    '@/./+/-/_ only.'),
        validators=[
            validators.RegexValidator(r'^[\w.@+-]+$',
                                      _('Enter a valid username. '
                                        'This value may contain only letters, numbers '
                                        'and @/./+/-/_ characters.'), 'invalid'),
        ],
        error_messages={
            'unique': _("A user with that username already exists."),
        })
    password = models.CharField(_('password'), max_length=128)
    role = models.CharField(max_length=50)
    email = models.EmailField(_('email address'), blank=True)
    is_super_user = models.BooleanField(_('super user'), default=False,
        help_text=_('Designates whether the user can have access to multiple countries site.'))
    notify_on_log_create = models.BooleanField(_('notify on new log'), default=False,
        help_text=_('If the user prefers to get notification or not'))


class Country(models.Model):
	name = models.CharField(max_length=50)
	country_code = models.CharField(max_length=3)
	language = models.CharField(max_length=6)

	class Meta:
		verbose_name_plural = "Countries"

	def __unicode__(self):
		return self.name

class RadioStation(models.Model):
	name = models.CharField(max_length=50)
	country = models.ForeignKey('Country',null=True,blank=True)
	city = models.CharField(max_length=50,null=True)
	phone_number = models.CharField(max_length=50,null=True,blank=True)
	email = models.EmailField(max_length=50,null=True,blank=True)
	uliza_password = models.CharField(max_length=50,null=True,blank=True)
	website = models.CharField(max_length=50,null=True,blank=True)
	manager = models.CharField(max_length=50,null=True,blank=True)

	frequency = models.CharField(max_length=50,null=True,blank=True)
	tower_location = models.CharField(max_length=50,null=True,blank=True)
	tower_height = models.CharField(max_length=50,null=True,blank=True)
	transmission_power = models.CharField(max_length=50,null=True,blank=True)
	transmission_gain = models.CharField(max_length=50,null=True,blank=True)

	lattitude = models.FloatField(null=True,blank=True)
	longitude = models.FloatField(null=True,blank=True)
	frequency = models.CharField(max_length=100,null=True,blank=True)
	telerivet_project_code = models.CharField(max_length=50,null=True,blank=True)

	# Time track
	last_updated_at = models.DateTimeField(auto_now=True)
	created_at = models.DateTimeField(auto_now_add=True)

	def __unicode__(self):
		return self.name


def filename(instance, filename):
	return 'FRI-LOG-'+str(instance.program.name)+'-'+str(instance.week)+'.mp3'

class Log(models.Model):
	program = models.ForeignKey("Program")
	presenter = models.ForeignKey("Presenter",blank=True,null=True)
	saved_by = models.ForeignKey(User,blank=True,null=True)
	topic = models.CharField(max_length=30,null=True,blank=True)

	focus_statement = models.TextField(null=True,blank=True)
	ict = models.TextField(blank=True,null=True)
	duration = models.IntegerField(null=True,blank=True)
	week = models.IntegerField(null=True,blank=True)

	# Format options
	studio_interviews = models.BooleanField(default=False)
	field_interviews = models.BooleanField(default=False)
	panel = models.BooleanField(default=False)
	community_discussion = models.BooleanField(default=False)
	phone_in = models.BooleanField(default=False)
	vox_pop = models.BooleanField(default=False)
	mini_documentary = models.BooleanField(default=False)
	talk_tape = models.BooleanField(default=False)
	question_answer = models.BooleanField(default=False)
	case_study = models.BooleanField(default=False)

	postpone = models.BooleanField(default=False)
	postponed_for = models.TextField(blank=True,null=True)

	email = models.TextField(blank=True,null=True,default=None)

	recording = models.FileField(upload_to='/FRI-LOG',storage=GDRIVE_STORAGE, null=True,blank=True)
	recording_backup = models.FileField(null=True,blank=True)
	recording_saved = models.BooleanField(default=True)
	offset = models.PositiveIntegerField(default=0)

	# Time track
	last_updated_at = models.DateTimeField(auto_now=True)
	created_at = models.DateTimeField(auto_now_add=True)

	def __unicode__(self):
		return self.topic

	def close_file(self):
		file_ = self.recording_backup
		while file_ is not None:
			file_.close()
			file_ = getattr(file_, 'file', None)

	def append_chunk(self, chunk, chunk_size=None, save=True):
		self.close_file()
		self.recording_backup.open(mode='ab')  # mode = append+binary
		# We can use .read() safely because chunk is already in memory
		self.recording_backup.write(chunk.read())
		if chunk_size is not None:
			self.offset += chunk_size
		elif hasattr(chunk, 'size'):
			self.offset += chunk.size
		else:
			self.offset = self.file.size
		self._md5 = None  # Clear cached md5
		if save:
			self.save()
		self.close_file()  # Flush

	def rename(self):
		import os
		if (self.recording_backup):
			old_path = self.recording_backup.path
			self.recording_backup.name = 'FRI-LOG-'+self.program.name+'-'+str(self.week)+'.mp3'

			os.rename(old_path, self.recording_backup.path)
			self.save()

class Comment(models.Model):
	content = models.TextField()
	log = models.ForeignKey('Log')
	user = models.ForeignKey(User)

	last_updated_at = models.DateTimeField(auto_now=True)
	created_at = models.DateTimeField(auto_now_add=True)


class Project(models.Model):
	name = models.CharField(max_length=50)
	country = models.ForeignKey('Country')
	doner = models.CharField(max_length=50)
	focus = models.CharField(max_length=50)

	start_date = models.DateField(null=True)
	end_date = models.DateField(null=True)

	# Time track
	last_updated_at = models.DateTimeField(auto_now=True)
	created_at = models.DateTimeField(auto_now_add=True)

	def __unicode__(self):
		return self.name

languages = (
	('en-us', 'English'),
	('pt-mz', 'Portuguese'),
	('am-et', 'Amharic'),
	('fr-fr', 'Francais')
)

class Presenter(models.Model):

	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en-us',choices=languages)

	def __unicode__(self):
		return self.user.username

class Group_account(models.Model):
	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	members = models.ManyToManyField('Presenter',blank=True)
	language = models.CharField(max_length=6,default='en-us',choices=languages)

	def __unicode__(self):
		return self.user.username


class Knowledge_partner(models.Model):

	user = models.ForeignKey(User)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	organization = models.CharField(max_length=64,null=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en-us',choices=languages)
	country = models.ForeignKey('Country', null=True)


	def __unicode__(self):
		return self.user.first_name

class Administrator(models.Model):
	user = models.ForeignKey(User)
	country = models.ForeignKey(Country)
	job_description = models.CharField(null=True,blank=True,max_length=80)
	notify_signup = models.BooleanField(default=True)
	notify_log_submission = models.BooleanField(default=True)
	notify_daily_schedule = models.BooleanField(default=False)
	language = models.CharField(max_length=6,default='en-us',choices=languages)

	def __unicode__(self):
		return self.user.username


# Contact
class Contact(models.Model):
	user_id = models.CharField(null=True, blank=True, max_length=120)
	radio_station = models.IntegerField(null=True, blank=True, default=None)
	first_name = models.CharField(null=True, blank=True, max_length=30)
	last_name = models.CharField(null=True, blank=True, max_length=30)
	job_title = models.CharField(null=True,blank=True, max_length=100)
	organization = models.CharField(null=True, blank=True, max_length=100)
	phone_number = models.CharField(max_length=50,null=True,blank=True)
	organization = models.CharField(max_length=64,null=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en-us',choices=languages)
	country = models.ForeignKey('Country', null=True)


	def __unicode__(self):
		return self.first_name


class Program(models.Model):

	days = (
	    ('Mon', 'Monday'),
	    ('Tue', 'Tuesday'),
	    ('Wed', 'Wednsday'),
	    ('Thu', 'Thursday'),
	    ('Fri', 'Friday'),
	    ('Sat', 'Saturday'),
	    ('Sun', 'Sunday'),
	)

	name = models.CharField(max_length=50)
	public_name = models.CharField(null=True,blank=True,max_length=50)
	radio_station = models.ForeignKey('RadioStation')
	project = models.ForeignKey('Project')
	program_type = models.CharField(null=True,blank=True,max_length=50)

	confirmed_program_time = models.BooleanField(default=False)
	uliza = models.CharField(null=True,blank=True,max_length=50)

	from django.utils import timezone
	start_date = models.DateTimeField(default=timezone.now)
	end_date = models.DateField(null=True)

	repeat_week_day = models.CharField(max_length=3,null=True,blank=True,choices=days)
	repeat_start_time = models.TimeField(null=True,blank=True)
	duration = models.IntegerField(default=30)

	weeks = models.IntegerField()

	access = models.ManyToManyField(User,blank=True)

	# Time track
	last_updated_at = models.DateTimeField(auto_now=True)
	created_at = models.DateTimeField(auto_now_add=True)

	def weeks_aired(self):
		logs = Log.objects.filter(program=self,postpone=False)
		return logs.count()

	def weeks_left(self):
		from django.utils import timezone
		today = timezone.now()
		start_date = self.start_date

		if today<start_date:
			today = start_date
		week = self.end_date - today.date()
		week = week.days/7 + 1

		if week < 1:
			week = 0
		return week


	def get_status(self):

		if(not self.confirmed_program_time):
			return 'Schedule not confirmed '
		elif self.weeks_left == self.weeks:
			return 'Not started'
		elif(self.weeks_left < 1):
			return 'Ended'
		else:
			return 'Running'

		return ''

	status=property(get_status)
	weeks_left = property(weeks_left)

	def __unicode__(self):
		return self.name
