from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Presenter
from log_app.serializers import PresenterSerializer

class PresenterGet(generics.ListCreateAPIView):

    queryset = Presenter.objects.all()
    model = Presenter
    serializer_class = PresenterSerializer
    filter_fields = ['id','radio_station']

class PresenterEntity(generics.RetrieveUpdateAPIView):

    queryset = Presenter.objects.all()
    model = Presenter
    serializer_class = PresenterSerializer
    lookup_field = 'id'