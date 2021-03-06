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

    premium_account = models.BooleanField(default=False)

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
    code = models.CharField(max_length=100)
    name = models.CharField(max_length=50)
    country = models.ForeignKey('Country')
    doner = models.CharField(max_length=50)
    focus = models.TextField()
    reference_links = models.TextField(null=True,blank=True)
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

# Depricated - now in contacts with role
class Presenter(models.Model):

	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en',choices=languages)

	def __unicode__(self):
		return self.user.username

# Depricated - now in contacts with role
class Group_account(models.Model):
	user = models.ForeignKey(User)
	radio_station = models.ForeignKey(RadioStation,null=True)
	members = models.ManyToManyField('Presenter',blank=True)
	language = models.CharField(max_length=6,default='en',choices=languages)

	def __unicode__(self):
		return self.user.username


# Depricated - now in contacts with role
class Knowledge_partner(models.Model):

	user = models.ForeignKey(User)
	phone_number = models.CharField(max_length=50,null=True,blank=True,unique=True)
	organization = models.CharField(max_length=64,null=True)
	role = models.CharField(max_length=64,null=True)
	language = models.CharField(max_length=6,default='en',choices=languages)
	country = models.ForeignKey('Country', null=True)


	def __unicode__(self):
		return self.user.first_name

# Depricated - now in contacts with role
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
        ('gender_specialist', 'Gender Specialist'),
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


	backup_status = (
	    ('none', 'None'),
	    ('removed', 'Removed'),
	    ('zip', 'Zip'),
	)

	name = models.CharField(max_length=50)
	public_name = models.CharField(null=True,blank=True,max_length=50)
	radio_station = models.ForeignKey('RadioStation')
	project = models.ForeignKey('Project')
	program_type = models.CharField(null=True,blank=True,max_length=50)

	confirmed_program_time = models.BooleanField(default=False)
	uliza = models.CharField(null=True,blank=True,max_length=80)

	from django.utils import timezone
	start_date = models.DateTimeField(default=timezone.now)
	end_date = models.DateField(null=True)

	repeat_week_day = models.CharField(max_length=3,null=True,blank=True,choices=days)
	repeat_start_time = models.TimeField(null=True,blank=True)
	duration = models.IntegerField(default=30)

	weeks = models.IntegerField()

	access = models.ManyToManyField(Contact,blank=True)
	media_backup_status = models.CharField(max_length=15,default='none',choices=backup_status)

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

class BroadcasterResource(models.Model):
    name = models.CharField(max_length=100)
    name_fr = models.CharField(max_length=100)
    name_pt = models.CharField(max_length=100)
    name_am = models.CharField(max_length=100)
    description = models.TextField(null=True,blank=True)

class Log(models.Model):
    program = models.ForeignKey("Program")
    saved_by = models.ForeignKey(Contact,blank=True,null=True)
    topic = models.CharField(max_length=100,null=True,blank=True)

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
    star_audio = models.BooleanField(default=False)
    recording_backup = models.FileField(null=True,blank=True)
    recording_saved = models.BooleanField(default=True)
    offset = models.PositiveIntegerField(default=0)

    broadcaster_resource = models.ForeignKey(BroadcasterResource,blank=True,null=True)
    link_to_resource = models.URLField(max_length=400,null=True,blank=True)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def __unicode__(self):
    	return self.topic or u'None'

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
            old_path = self.recording_backup.path.encode('utf8')
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
    telegram_username = models.CharField(max_length=80, null=True)

    training_call = models.BooleanField(default=False)

    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def save(self,*args,**kwargs):
        comment_already_exists = Comment.objects.filter(log=self.log,training_call=self.training_call,content=self.content)
        if comment_already_exists:
            return
        else:
            return super(Comment, self).save(*args, **kwargs)



# Format is a radio format
"""
The new formats (non legacy) is a radio format and
also can be just a checklist category
"""
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
    project_related = models.BooleanField(default=False)
    projects = models.ManyToManyField('Project',blank=True)

    def __unicode__(self):
    	return self.name

checklist_level = (
    ('best', 'Best'),
    ('good', 'Good'),
    ('better', 'Better')
)

class Checklist(models.Model):
    radio_format = models.ForeignKey('Format')
    level = models.CharField(max_length=6,default='good',choices=checklist_level)
    description = models.TextField(null=True,blank=True)
    description_fr = models.TextField(null=True,blank=True)
    description_pt = models.TextField(null=True,blank=True)
    description_am = models.TextField(null=True,blank=True)
    gender_responsive = models.BooleanField(default=False)

    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)

    def __unicode__(self):
    	return self.description


class Review(models.Model):
    log = models.ForeignKey('log')
    reviewer = models.ForeignKey(Contact)
    draft = models.BooleanField(default=False)
    checklists = models.ManyToManyField('Checklist',blank=True)
    void_formats = models.ManyToManyField('Format',blank=True)

    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)

    def calculate_score(self):
        formats = Format.objects.filter(always_checked=True).values_list('id',flat=True)
        checklists = Checklist.objects.filter(radio_format__in=formats).values_list('id','level')
        checklists = list(checklists)
        checklists+list(self.log.formats.values_list('checklist__id','checklist__level'))

        review_checklists = self.checklists.values_list('id','level')
        total = 0
        total_score = 0

        for checklist in checklists:
            score =0
            if checklist[1] =='best':
                score = 3
            elif checklist[1] == 'better':
                score = 2
            else:
                score = 1

            total = total+score
            if checklist in review_checklists:
                total_score = total_score+score

        total_score = float(total_score)/total
        total_score = total_score*100

        return total_score


pod_status = (
    ('inactive', 'Inactive'),
    ('requested', 'Requested'),
    ('active', 'Active'),
    ('recheck', 'Recheck'),
)

class Podcast(models.Model):
    spreaker_show_id = models.CharField(null=True, blank=True, max_length=120)
    title = models.CharField(max_length=50)
    radio_station = models.ForeignKey('RadioStation')

    # for the following fields default radio station website,name, email and country language respectively
    website = models.CharField(max_length=50,null=True,blank=True)
    owner = models.CharField(max_length=80,null=True,blank=True)
    owner_email = models.EmailField(max_length=50,null=True,blank=True)
    language = models.CharField(max_length=6,default="en")

    description = models.TextField(null=True,blank=True,default="None")
    category = models.CharField(max_length=60,null=True,blank=True)
    explicit = models.BooleanField(default=False)

    # default is the spreaker image
    image = models.CharField(max_length=400,null=True,blank=True)
    custom_image = models.CharField(null=True, blank=True,max_length=200)

    # listener engagement
    twitter_url = models.CharField(max_length=400,null=True,blank=True)
    facebook_url = models.CharField(max_length=400,null=True,blank=True)
    itunes = models.CharField(null=True, blank=True,max_length=100)
    skype_name = models.CharField(null=True, blank=True,max_length=100)
    text_number = models.CharField(null=True, blank=True,max_length=100)
    telephone_number = models.CharField(max_length=50,null=True,blank=True)

    apple_podcasts_status = models.CharField(max_length=15,default='inactive',choices=pod_status)
    spotify_status = models.CharField(max_length=15,default='inactive',choices=pod_status)
    google_podcasts_status = models.CharField(max_length=15,default='inactive',choices=pod_status)
    podcast_addict_status = models.CharField(max_length=15,default='inactive',choices=pod_status)
    amazon_music_status = models.CharField(max_length=15,default='inactive',choices=pod_status)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def save(self,*args,**kwargs):
        if self.id:
            from django.utils.translation import gettext
            pod = Podcast.objects.get(id=self.id)
            update = {'apple_podcasts_status':None, 'spotify_status':None, 'google_podcasts_status':None,'amazon_music_status':None,'podcast_addict_status':None}
            notification_for_staff = False

            # send notifications and log the changes
            if not pod.apple_podcasts_status == self.apple_podcasts_status:
                update['apple_podcasts_status'] = self.apple_podcasts_status

                if self.apple_podcasts_status == 'requested':
                    notification_for_staff = True

            if not pod.spotify_status == self.spotify_status:
                update['spotify_status'] = self.spotify_status

                if self.spotify_status == 'requested':
                    notification_for_staff = True

            if not pod.google_podcasts_status == self.google_podcasts_status:
                update['google_podcasts_status'] = self.google_podcasts_status

                if self.google_podcasts_status == 'requested':
                    notification_for_staff = True

            if not pod.amazon_music_status == self.amazon_music_status:
                update['amazon_music_status'] = self.amazon_music_status

                if self.amazon_music_status == 'requested':
                    notification_for_staff = True

            if not pod.podcast_addict_status == self.podcast_addict_status:
                update['podcast_addict_status'] = self.podcast_addict_status

                if self.podcast_addict_status == 'requested':
                    notification_for_staff = True

            content = ''
            translation = {'apple_podcasts_status':'Apple podcasts','spotify_status':'Spotify','google_podcasts_status':'Google podcasts','amazon_music_status':'Amazon music','podcast_addict_status':'Podcast addict'}
            for platform in update:
                if update[platform]:
                    if content:
                        content = content+', '+ translation[platform]
                    else:
                        content = translation[platform]

            url_model = ''
            if notification_for_staff:
                contacts = Contact.objects.filter(is_admin=True)
                heading = self.title+' ('+ self.radio_station.name+')'
                url_model = 'podcasts:distribution'
                message = ' requested'
            else:
                message = ' status updated'
                heading = self.title
                url_model = 'podcasts:manage'
                contacts = Contact.objects.filter(radio_station=self.radio_station.id)

            for contact in contacts:
                final_content = content +' '+gettext(message)
                Notification.objects.create(url_model=url_model,link=self.id,sent_to=contact,content=final_content,heading=heading)

        return super(Podcast, self).save(*args, **kwargs)


class PodEpisode(models.Model):
    spreaker_episode_id = models.CharField(null=True, blank=True, max_length=120)
    podcast = models.ForeignKey('Podcast')

    title = models.CharField(null=True,blank=True,max_length=150)
    description = models.TextField(null=True,blank=True,default="None")
    audio_file = models.FileField(null=True,blank=True)
    audio_saved = models.BooleanField(default=True)
    public = models.BooleanField(default=False)
    audio_file_offset = models.PositiveIntegerField(default=0)
    spreaker_audio_url = models.TextField(null=True,blank=True)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def close_file(self):
    	file_ = self.audio_file
    	while file_ is not None:
    		file_.close()
    		file_ = getattr(file_, 'file', None)

    def append_chunk(self, chunk, chunk_size=None, save=True):
    	self.close_file()
    	self.audio_file.open(mode='ab')  # mode = append+binary
    	# We can use .read() safely because chunk is already in memory
    	self.audio_file.write(chunk.read())
    	if chunk_size is not None:
    		self.audio_file_offset += chunk_size
    	elif hasattr(chunk, 'size'):
    		self.audio_file_offset += chunk.size
    	else:
    		self.audio_file_offset = self.file.size
    	self._md5 = None  # Clear cached md5
    	if save:
    		self.save()
    	self.close_file()  # Flush

    def rename(self):
        import os
        if (self.audio_file):
            old_path = self.audio_file.path.encode('utf8')
            self.audio_file.name = 'Uliza-log-'+self.podcast.title.encode('utf-8')+'-'+str(self.id)+'.mp3'

            try:
                self.audio_file.name.encode('ascii')
            except:
                self.audio_file.name='Uliza-log-ID'+str(self.id)+'.mp3'

            os.rename(old_path, self.audio_file.path)
            self.save()

class PodDistributionLog(models.Model):
    podcast = models.ForeignKey('Podcast')
    triggered_by =  models.ForeignKey('Contact',null=True)
    note = models.TextField(null=True,blank=True,default="None")

    apple_podcasts_status = models.CharField(max_length=15,default=None,null=True,choices=pod_status)
    spotify_status = models.CharField(max_length=15,default=None,null=True,choices=pod_status)
    google_podcasts_status = models.CharField(max_length=15,default=None,null=True,choices=pod_status)
    podcast_addict_status = models.CharField(max_length=15,default=None,null=True,choices=pod_status)
    amazon_music_status = models.CharField(max_length=15,default=None,null=True,choices=pod_status)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)


class Notification(models.Model):
    sent_to =  models.ForeignKey('Contact')
    heading = models.CharField(max_length=80,null=True,blank=True)
    content = models.TextField(null=True,blank=True,default="None")
    seen = models.BooleanField(default=False)
    url_model = models.CharField(max_length=80,null=True,blank=True)
    link = models.IntegerField(null=True,blank=True)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)
