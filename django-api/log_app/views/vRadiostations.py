from django.http import JsonResponse
from rest_framework.response import Response
from rest_framework.pagination import PageNumberPagination
from rest_framework.decorators import api_view
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import RadioStation,Program,Project
from log_app.serializers import RadioStationSerializer,ProjectSerializer
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework import filters

class RadioStationGet(generics.ListCreateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    filter_fields = ['id','country','group_account_id']
    filter_backends = (filters.SearchFilter,DjangoFilterBackend)
    search_fields = ('name','city','country__name')

class RadioStationEntity(generics.RetrieveUpdateAPIView):

    queryset = RadioStation.objects.all()
    model = RadioStation
    serializer_class = RadioStationSerializer
    lookup_field = 'id'

@api_view(['GET'])
def radio_station_projects(request,id):
    context = {'request', request}
    paginator = PageNumberPagination()

    if 'page_size' in request.GET:
        paginator.page_size = request.GET['page_size']
    else:
        paginator.page_size = 4

    programs = Program.objects.filter(radio_station__id=id).values_list('project__id',flat=True)
    projects = Project.objects.filter(pk__in=programs)
    projects = paginator.paginate_queryset(projects,request)
    projects = ProjectSerializer(projects,many=True)

    return paginator.get_paginated_response(projects.data)
