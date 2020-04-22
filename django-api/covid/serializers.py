from rest_framework import serializers
from covid.models import *

class ContentSerializer(serializers.ModelSerializer):
	class Meta:
		model = Content
		fields = "__all__"


class ChatUserSerializer(serializers.ModelSerializer):
	class Meta:
		model = ChatUser
		fields = "__all__"

class QuestionSerializer(serializers.ModelSerializer):
	question_from = serializers.CharField(source='chat_user.full_name',read_only=True)
	country = serializers.CharField(source='chat_user.country',read_only=True)
	language = serializers.CharField(source='chat_user.language',read_only=True)
	radio_station = serializers.CharField(source='chat_user.radio_station',read_only=True)

	class Meta:
		model = Question
		fields = "__all__"
