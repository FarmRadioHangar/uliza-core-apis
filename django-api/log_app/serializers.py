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

"""
Example

	id = serializers.IntegerField(read_only=True)
    title = serializers.CharField(required=False, allow_blank=True, max_length=100)
    code = serializers.CharField(style={'base_template': 'textarea.html'})
    linenos = serializers.BooleanField(required=False)
    language = serializers.ChoiceField(choices=LANGUAGE_CHOICES, default='python')
    style = serializers.ChoiceField(choices=STYLE_CHOICES, default='friendly')
"""

class RadioStationSerializer(serializers.ModelSerializer):
	# name = serializers.CharField()
	# country = serializers.ForeignKey('Country',null=True,blank=True)
	# city = serializers.CharField()
	# phone_number = serializers.CharField()
	# email = serializers.EmailField()
	# uliza_password = serializers.CharField()
	# website = serializers.CharField()
	# manager = serializers.CharField()

	# frequency = serializers.CharField()
	# tower_location = serializers.CharField()
	# tower_height = serializers.CharField()
	# transmission_power = serializers.CharField()
	# transmission_gain = serializers.CharField()

	# lattitude = serializers.FloatField()
	# longitude = serializers.FloatField()
	# frequency = serializers.CharField()
	# telerivet_project_code = serializers.CharField()

	class Meta:
		model = RadioStation
		fields = ('name', 'city', 'phone_number', 'email', 'uliza_password',
				 'frequency', 'tower_location', 'tower_height', 'transmission_power', 'transmission_gain','country',
				 'lattitude','longitude','frequency')
		ordering = ('created_at',)

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

		if instance:
			return instance

