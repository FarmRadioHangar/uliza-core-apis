from rest_framework import serializers
from log_app.models import RadioStation

def updater(instance, validated_data, attributes):
	try:
		for attr in attributes:
			setattr(instance, attr, getattr(validated_data, attr))

		instance.save()
		return instance
	except:
		print sys.exec_info[0]

		return False

class RadioStationSerializer(serializers.ModelSerializer):

	class Meta:
		model = RadioStation
		fields = "__all__"

