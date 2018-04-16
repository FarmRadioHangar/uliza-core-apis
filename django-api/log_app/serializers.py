from rest_framework import serializers
from log_app.models import *

class RadioStationSerializer(serializers.ModelSerializer):

	class Meta:
		model = RadioStation
		fields = "__all__"

class ProjectSerializer(serializers.ModelSerializer):
	class Meta:
		model = Project
		fields = "__all__"

class ProgramSerializer(serializers.ModelSerializer):
	radio_station__name = serializers.CharField(source='radio_station.name',read_only=True)
	project__name = serializers.CharField(source='project.name',read_only=True)
	country = serializers.CharField(source='radio_station.country.id',read_only=True)
	country_name = serializers.CharField(source='radio_station.country',read_only=True)

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
		fields = "__all__"

class CommentSerializer(serializers.ModelSerializer):
	class Meta:
		model = Comment
		fields = "__all__"


class ContactSerializer(serializers.ModelSerializer):
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

class ChecklistSerializer(serializers.ModelSerializer):
	class Meta:
		model = Checklist
		fields = "__all__"

class FormatSerializer(serializers.ModelSerializer):
	class Meta:
		model = Format
		fields = "__all__"
