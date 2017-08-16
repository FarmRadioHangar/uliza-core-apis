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
		
	def create(self, validated_data):
		"""
		Create and return a new `RadioStation` instance, given the validated data.
		"""
		return RadioStation.objects.create(**validated_data)

	def update(self, instance, validated_data):
		"""
		Update and return an existing `RadioStation` instance, given the validated data.
		"""
		instance = updater(instance, validated_data, 
				['name','country','city','phone_number', 'email',
				 'uliza_password','website','manager','manager', 
				 'frequency', 'tower_location', 'tower_height'
				])

		return instance

