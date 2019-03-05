from django.http import JsonResponse
from rest_framework.response import Response
from rest_framework.decorators import api_view
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import RadioStation,Program,Project
from log_app.serializers import RadioStationSerializer,ProjectSerializer

class RadioStationGet(generics.ListCreateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    filter_fields = ['id','country','group_account_id']

class RadioStationEntity(generics.RetrieveUpdateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    lookup_field = 'id'

@api_view(['GET'])
def radion_station_projects(request,id):
    programs = Program.objects.filter(radio_station__id=id).values_list('project__id',flat=True)
    projects = Project.objects.filter(pk__in=programs)
    projects = ProjectSerializer(projects,many=True)

    return Response(projects.data)
