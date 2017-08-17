from log_app.models import RadioStation
from log_app.serializers import RadioStationSerializer

from django.http import HttpResponse, JsonResponse


def root(request):
	"""
	List of all stations and create a new station
	"""
	if request.method == 'GET':
		stations = RadioStation.objects.all()
		serializer = RadioStationSerializer(stations, many=True)

		return JsonResponse(serializer.data, safe=False)



	return False


