from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Group_account
from log_app.serializers import GroupAccountSerializer

class GroupAccountGet(generics.ListCreateAPIView):

    queryset = Group_account.objects.all()
    model = Group_account
    serializer_class = GroupAccountSerializer
    filter_fields = ['id']

class GroupAccountEntity(generics.RetrieveUpdateAPIView):

    queryset = Group_account.objects.all()
    model = Group_account
    serializer_class = GroupAccountSerializer
    lookup_field = 'id'