from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Log
from log_app.serializers import LogSerializer

from rest_framework import filters
from django_filters.rest_framework import DjangoFilterBackend

class LogGet(generics.ListCreateAPIView):

	queryset = Log.objects.all()
	model = Log
	serializer_class = LogSerializer
	filter_fields = ['id','program','program__radio_station__country','postpone']
	filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
	ordering_fields = ('week','id')

	def get_queryset(self):
		"""
		This view should return a list of all the purchases
		for the currently authenticated user.
		"""
		pk_list = self.request.GET.get('program__in')
		if pk_list:
			pk_list = pk_list.split(',')
			logs = Log.objects.filter(program__in=pk_list)
		else:
			logs = Log.objects.all()

		return logs


class LogEntity(generics.RetrieveUpdateAPIView):

    queryset = Log.objects.all()
    model = Log
    serializer_class = LogSerializer
    lookup_field = 'id'