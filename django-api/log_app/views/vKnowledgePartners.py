from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Knowledge_partner
from log_app.serializers import KnowledgePartnerSerializer

class KnowledgePartnerGet(generics.ListCreateAPIView):

    queryset = Knowledge_partner.objects.all()
    model = Knowledge_partner
    serializer_class = KnowledgePartnerSerializer
    filter_fields = ['id','country']

class KnowledgePartnerEntity(generics.RetrieveUpdateAPIView):

    queryset = Knowledge_partner.objects.all()
    model = Knowledge_partner
    serializer_class = KnowledgePartnerSerializer
    lookup_field = 'id'