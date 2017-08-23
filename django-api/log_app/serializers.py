from rest_framework import serializers
from log_app.models import *

class RadioStationSerializer(serializers.ModelSerializer):

	class Meta:
		model = RadioStation
		fields = "__all__"


class ProgramSerializer(serializers.ModelSerializer):
	radio_station = RadioStationSerializer(read_only=True)
	class Meta:
		model = Program
		fields = "__all__"

class CountrySerializer(serializers.ModelSerializer):

	class Meta:
		model = Country
		fields = "__all__"

class LogSerializer(serializers.ModelSerializer):
	class Meta:
		model = Log
		fields = "__all__"

class CommentSerializer(serializers.ModelSerializer):
	class Meta:
		model = Comment
		fields = "__all__"

class ProjectSerializer(serializers.ModelSerializer):
	class Meta:
		model = Project
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