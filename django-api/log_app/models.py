# -*- coding: utf-8 -*-
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

	gbb = models.BooleanField(default=False)

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
    group_account_id = models.CharField(null=True, blank=True, max_length=120)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def __unicode__(self):
        return self.name

class RadioTransmission(models.Model):
    radio_station = models.ForeignKey('RadioStation')
    frequency = models.CharField(null=True, blank=True,max_length=80)
    gain = models.CharField(null=True, blank=True,max_length=80)
    height = models.CharField(null=True, blank=True,max_length=80)
    power = models.CharField(null=True, blank=True,max_length=80)
    coordinates = models.CharField(null=True, blank=True,max_length=120)


def filename(instance, filename):
	return 'FRI-LOG-'+str(instance.program.name)+'-'+str(instance.week)+'.mp3'


class Project(models.Model):
    name = models.CharField(max_length=50)
    country = models.ForeignKey('Country')
    doner = models.CharField(max_length=50)
    focus = models.CharField(max_length=50)
    image = models.CharField(null=True, blank=True,max_length=100)

    start_date = models.DateField(null=True)
    end_date = models.DateField(null=True)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def __unicode__(self):
        return self.name

languages = (
	('en', 'English'),
	('pt', 'Portuguese'),
	('am', 'Amharic'),
	('fr', 'Francais')
)

class Presenter(models.Model):

	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en',choices=languages)

	def __unicode__(self):
		return self.user.username

class Group_account(models.Model):
	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	members = models.ManyToManyField('Presenter',blank=True)
	language = models.CharField(max_length=6,default='en',choices=languages)

	def __unicode__(self):
		return self.user.username


class Knowledge_partner(models.Model):

	user = models.ForeignKey(User)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	organization = models.CharField(max_length=64,null=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en',choices=languages)
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
	language = models.CharField(max_length=6,default='en',choices=languages)

	def __unicode__(self):
		return self.user.username


# Contact
class Contact(models.Model):
    roles = (
        ('unknown', 'Unknown'),
        ('staff', 'Staff'),
        ('consultant', 'Consultant'),
        ('broadcaster', 'Broadcaster'),
        ('project_partner', 'Project Partner'),
        ('knowledge_partner', 'Knowledge Partner'),
    )
    user_id = models.CharField(null=True, blank=True, max_length=120)
    radio_station = models.IntegerField(null=True, blank=True, default=None)
    first_name = models.CharField(null=True, blank=True, max_length=30)
    last_name = models.CharField(null=True, blank=True, max_length=30)
    job_title = models.CharField(null=True,blank=True, max_length=100)
    organization = models.CharField(null=True, blank=True, max_length=100)
    phone_number = models.CharField(max_length=50,null=True,blank=True)
    email = models.EmailField(max_length=50,null=True,blank=True)
    role = models.CharField(max_length=64,null=True,choices=roles)
    language = models.CharField(max_length=6,default='en',choices=languages)
    country = models.ForeignKey('Country', null=True)

    is_superuser = models.BooleanField(default=False)
    is_admin = models.BooleanField(default=False)
    blocked = models.BooleanField(default=False)

    notify_on_log_create = models.BooleanField(default=False)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

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

	access = models.ManyToManyField(Contact,blank=True)

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

class Log(models.Model):
    program = models.ForeignKey("Program")
    saved_by = models.ForeignKey(Contact,blank=True,null=True)
    topic = models.CharField(max_length=30,null=True,blank=True)

    focus_statement = models.TextField(null=True,blank=True)
    ict = models.TextField(blank=True,null=True)
    duration = models.IntegerField(null=True,blank=True)
    week = models.IntegerField(null=True,blank=True)

    # Format options
    formats = models.ManyToManyField('Format',blank=True)

    postpone = models.BooleanField(default=False)
    postponed_for = models.TextField(blank=True,null=True)

    email = models.TextField(blank=True,null=True,default=None)

    gdrive = models.FileField(upload_to='/FRI-LOG',storage=GDRIVE_STORAGE, null=True,blank=True)
    gdrive_available = models.BooleanField(default=False)
    gdrive_url = models.URLField(max_length=400,null=True,blank=True)
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
    		self.recording_backup.name = 'Uliza-log-'+self.program.name.encode('utf-8')+'-'+str(self.week)+'.mp3'

    		try:
			self.recording_backup.name.encode('ascii')
		except:
			self.recording_backup.name='Uliza-log-ID'+str(self.id)+'-W'+str(self.week)+'.mp3' 
			
    		os.rename(old_path, self.recording_backup.path)
    		self.save()

class Comment(models.Model):
    content = models.TextField()
    log = models.ForeignKey('Log')

    # If null the comment is from the radio station broadcasting group
    contact = models.ForeignKey('Contact',null=True)

    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

class Format(models.Model):
    name = models.CharField(max_length=60)
    name_fr = models.CharField(null=True,blank=True,max_length=60)
    name_pt = models.CharField(null=True,blank=True,max_length=60)
    name_am = models.CharField(null=True,blank=True,max_length=60)
    description = models.TextField(null=True,blank=True)
    last_updated_at = models.DateTimeField(auto_now=True)

    # legacy formats that won't show up in the formats but is used for old Logs
    legacy = models.BooleanField(default=False)
    always_checked = models.BooleanField(default=False)

checklist_level = (
    ('best', 'Best'),
    ('good', 'Good'),
    ('better', 'Best')
)

class Checklist(models.Model):
    radio_format = models.ForeignKey('Format')
    level = models.CharField(max_length=6,default='good',choices=checklist_level)
    description = models.TextField(null=True,blank=True)
    description_fr = models.TextField(null=True,blank=True)
    description_pt = models.TextField(null=True,blank=True)
    description_am = models.TextField(null=True,blank=True)

    def __unicode__(self):
    	return self.description


class Review(models.Model):
    log = models.ForeignKey('log')
    reviewer = models.ForeignKey(Contact)
    draft = models.BooleanField(default=False)
    checklists = models.ManyToManyField('Checklist',blank=True)

    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)
