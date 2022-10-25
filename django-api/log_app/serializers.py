from rest_framework import serializers
from log_app.models import *

class RadioStationSerializer(serializers.ModelSerializer):
	class Meta:
		model = RadioStation
		fields = "__all__"

class ProjectSerializer(serializers.ModelSerializer):
	country__name = serializers.CharField(source='country.name',read_only=True)
	class Meta:
		model = Project
		fields = "__all__"

class BroadcasterResourceSerializer(serializers.ModelSerializer):
	class Meta:
		model = BroadcasterResource
		fields = "__all__"

class BroadcastLanguageSerializer(serializers.ModelSerializer):
	class Meta:
		model = BroadcastLanguage
		fields = "__all__"

class RadioTypeSerializer(serializers.ModelSerializer):
	class Meta:
		model = RadioType
		fields = "__all__"

class IndicatorSerializer(serializers.ModelSerializer):
	class Meta:
		model = Indicator
		fields = "__all__"

class TargetSerializer(serializers.ModelSerializer):
	indicator__order = serializers.CharField(source='index.order',read_only=True)
	class Meta:
		model = Target
		fields = "__all__"


class ReportSerializer(serializers.ModelSerializer):
	reported_by__first_name = serializers.CharField(source='reported_by.first_name',read_only=True)
	reported_by__last_name = serializers.CharField(source='reported_by.last_name',read_only=True)
	project__code = serializers.CharField(source='target.project.code',read_only=True)
	country = serializers.CharField(source='target.project.country',read_only=True)

	class Meta:
		model = Report
		fields = "__all__"


class ProgramSerializer(serializers.ModelSerializer):
	radio_station__name = serializers.CharField(source='radio_station.name',read_only=True)
	project__name = serializers.CharField(source='project.name',read_only=True)
	country = serializers.IntegerField(source='radio_station.country.id',read_only=True)
	country_name = serializers.CharField(source='radio_station.country',read_only=True)

	class Meta:
		model = Program
		fields = "__all__"

class ProgramRecordingSerializer(serializers.ModelSerializer):
	radio_station__name = serializers.CharField(source='radio_station.name',read_only=True)
	project__name = serializers.CharField(source='project.name',read_only=True)
	recordings = serializers.IntegerField()

	class Meta:
		model = Program
		fields = "__all__"


class CountrySerializer(serializers.ModelSerializer):
	radio_station__name = serializers.CharField(source='radio_station.name',read_only=True)
	program__name = serializers.CharField(source='program.name',read_only=True)

	class Meta:
			model = Country
			fields = "__all__"

class LogSerializer(serializers.ModelSerializer):
	project = serializers.IntegerField(source='program.project.id',read_only=True)
	program__name = serializers.CharField(source='program.name',read_only=True)

	class Meta:
		model = Log
		exclude = ('gdrive',)

class CommentSerializer(serializers.ModelSerializer):
	contact__last_name = serializers.CharField(source='contact.last_name',read_only=True)
	contact__first_name = serializers.CharField(source='contact.first_name',read_only=True)
	contact__job_title = serializers.CharField(source='contact.job_title',read_only=True)
	program_name = serializers.CharField(source='log.program.name',read_only=True)
	week = serializers.CharField(source='log.week',read_only=True)
	class Meta:
		model = Comment
		fields = ('week','program_name','content','log','contact__first_name','contact__last_name','contact__job_title','telegram_username','contact','last_updated_at','created_at','id','training_call')


class ContactSerializer(serializers.ModelSerializer):
	country__name = serializers.CharField(source='country.name',read_only=True)
	class Meta:
		model = Contact
		fields = "__all__"

class RadioTransmissionSerializer(serializers.ModelSerializer):
	class Meta:
		model = RadioTransmission
		fields = "__all__"

class Auth0UserSerializer(serializers.ModelSerializer):
	class Meta:
		model = Auth0User
		fields = "__all__"

class PresenterSerializer(serializers.ModelSerializer):
	class Meta:
		model = Presenter
		fields = "__all__"

class GroupAccountSerializer(serializers.ModelSerializer):
	class Meta:
		model = Group_account
		fields = "__all__"

class KnowledgePartnerSerializer(serializers.ModelSerializer):
	class Meta:
		model = Knowledge_partner
		fields = "__all__"

class AdministratorSerializer(serializers.ModelSerializer):
	class Meta:
		model = Administrator
		fields = "__all__"

class ReviewSerializer(serializers.ModelSerializer):
	class Meta:
		model = Review
		fields = "__all__"

class ReviewLogSerializer(serializers.ModelSerializer):
	log=LogSerializer()

	class Meta:
		model = Review
		fields = "__all__"

class ChecklistSerializer(serializers.ModelSerializer):
	class Meta:
		model = Checklist
		fields = "__all__"

class FormatSerializer(serializers.ModelSerializer):
	class Meta:
		model = Format
		fields = "__all__"

class PodcastSerializer(serializers.ModelSerializer):
	radio_station__name = serializers.CharField(source='radio_station.name',read_only=True)
	class Meta:
		model = Podcast
		fields = "__all__"

class PodEpisodeSerializer(serializers.ModelSerializer):
	class Meta:
		model = PodEpisode
		fields = "__all__"

class PodDistributionLogSerializer(serializers.ModelSerializer):
	first_name = serializers.CharField(source='triggered_by.first_name',read_only=True)
	last_name = serializers.CharField(source='triggered_by.last_name',read_only=True)
	class Meta:
		model = PodDistributionLog
		fields = "__all__"

class NotificationSerializer(serializers.ModelSerializer):
	class Meta:
		model = Notification
		fields = "__all__"

class PollSegmentSerializer(serializers.ModelSerializer):
	class Meta:
		model = PollSegment
		fields = "__all__"
