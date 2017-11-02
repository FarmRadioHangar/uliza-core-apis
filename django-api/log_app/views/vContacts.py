from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Contact
from log_app.serializers import ContactSerializer

class ContactGet(generics.ListCreateAPIView):

    queryset = Contact.objects.all()
    model = Contact
    serializer_class = ContactSerializer
    filter_fields = ['id']

class ContactEntity(generics.RetrieveUpdateAPIView):

    queryset = Contact.objects.all()
    model = Contact
    serializer_class = ContactSerializer
    lookup_field = 'id'
