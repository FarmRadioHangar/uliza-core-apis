from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Contact
from log_app.serializers import ContactSerializer

from rest_framework import status
from rest_framework.decorators import api_view
from rest_framework.response import Response


class ContactGet(generics.ListCreateAPIView):
    queryset = Contact.objects.all()
    model = Contact
    serializer_class = ContactSerializer
    filter_fields = ['id','user_id','role','country','radio_station']

class ContactEntity(generics.RetrieveUpdateAPIView):
    queryset = Contact.objects.all()
    model = Contact
    serializer_class = ContactSerializer

@api_view(['PATCH'])
def update_access(request,id):
    """
    Update the list of programs the contact is allowed to access
    """
    from log_app.models import Contact,Program

    contact = Contact.objects.get(pk=id)
    access = request.data['access'].split(',')
    programs = Program.objects.filter(access=contact)|Program.objects.filter(id__in=access)

    for p in programs:

    	if str(p.pk) in access:
    		p.access.add(contact.id)
        else:
    		p.access.remove(contact.id)

        p.save()

    return Response(access, status=status.HTTP_200_OK)
