from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Auth0User
from log_app.serializers import Auth0UserSerializer

import django_filters
from rest_framework import filters

class Auth0UserGet(generics.ListCreateAPIView):

    queryset = Auth0User.objects.all()
    model = Auth0User
    serializer_class = Auth0UserSerializer
    filter_fields = ['id']

class Auth0UserEntity(generics.RetrieveUpdateAPIView):
    queryset = Auth0User.objects.all()
    model = Auth0User
    serializer_class = Auth0UserSerializer
    lookup_field = 'id'
