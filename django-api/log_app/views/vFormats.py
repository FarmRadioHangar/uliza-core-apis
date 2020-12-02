from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Format
from log_app.serializers import FormatSerializer
import django_filters
from rest_framework import filters
from rest_framework.decorators import api_view
from rest_framework.response import Response

class FormatGet(generics.ListCreateAPIView):
    queryset = Format.objects.all()
    model = Format
    serializer_class = FormatSerializer
    filter_fields = ['id','name','legacy','always_checked','project_related','projects']

    def get_queryset(self):
        pk_list = self.request.GET.get('pk_list')
        if pk_list:
            pk_list = pk_list.split(',')
            queryset = Format.objects.filter(pk__in=pk_list).order_by('name')
        else:
            queryset = Format.objects.all().order_by('name')
        return queryset


class FormatEntity(generics.RetrieveUpdateAPIView):
    queryset = Format.objects.all()
    model = Format
    serializer_class = FormatSerializer
    lookup_field = 'id'

@api_view(['PATCH'])
def update_access(request,id):
    """
    Update the list of projects the format is used in
    """
    from log_app.models import Project,Format

    format = Format.objects.get(pk=id)
    access = request.data['access'].split(',')
    projects = Project.objects.filter(id__in=access)

    for p in projects:
        format.projects.add(p)

    format.save()

    return Response(access, status=status.HTTP_200_OK)
