from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Checklist
from log_app.serializers import ChecklistSerializer
import django_filters
from rest_framework import filters

class ChecklistFilter(filters.FilterSet):
	created_at__lte = django_filters.Filter(name="created_at", lookup_expr='lte')
	created_at__gt = django_filters.Filter(name="created_at", lookup_expr='gt')
	class Meta:
		model = Checklist
		fields = ['id','level','radio_format','gender_responsive','created_at__lte','created_at__gt']

class ChecklistGet(generics.ListCreateAPIView):
    # queryset = Checklist.objects.all()
    model = Checklist
    serializer_class = ChecklistSerializer
    filter_class = ChecklistFilter
    filter_backends = (filters.OrderingFilter,filters.DjangoFilterBackend)
    ordering_fields = ('level','radio_format')

    def get_queryset(self):
        """
        This view should return a list of all the purchases
        for the currently authenticated user.
        """
        pk_list = self.request.GET.get('pk_list')
        format_list = self.request.GET.get('format_list')
        if pk_list:
            pk_list = pk_list.split(',')
            queryset = Checklist.objects.filter(pk__in=pk_list)
        else:
            if format_list:
                format_list = format_list.split(',')
                queryset = Checklist.objects.filter(radio_format__in=format_list)
            else:
                queryset = Checklist.objects.all()
        return queryset


class ChecklistEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Checklist.objects.all()
    model = Checklist
    serializer_class = ChecklistSerializer
    lookup_field = 'id'
