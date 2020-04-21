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
    class Meta:
        model = Question
        fields = "__all__"
