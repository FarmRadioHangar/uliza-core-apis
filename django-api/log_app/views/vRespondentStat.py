from rest_framework import generics
from rest_framework import filters
from log_app.models import RespondentStat
from django_filters.rest_framework import DjangoFilterBackend
from log_app.serializers import RespondentStatSerializer

class RespondentStatGet(generics.ListCreateAPIView):
    queryset = RespondentStat.objects.all()
    model = RespondentStat
    serializer_class = RespondentStatSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    filter_fields = ['id','episode_number','program']
    ordering_fields = ('episode_number','id')

class RespondentStatEntity(generics.UpdateAPIView):
    queryset = RespondentStat.objects.all()
    model = RespondentStat
    serializer_class = RespondentStatSerializer
    lookup_field = 'id'
