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
	station_name = serializers.CharField(source='radio_station.name',read_only=True)
	project_name = serializers.CharField(source='project.name',read_only=True)
	country = serializers.CharField(source='radio_station.country.id',read_only=True)

	class Meta:
		model = Program
		fields = "__all__"
	
	# def create(self, validated_data):
	# 	radio_station = validated_data.pop('radio_station')
	# 	radio_station = RadioStation.objects.create(**validated_data)
	# 	Profile.objects.create(user=user, **profile_data)
	# 	return user

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