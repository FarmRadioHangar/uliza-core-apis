from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from uliza.models import VotoWebhookLog
from uliza.serializers import VotoWebhookLogSerializer

class VotoWebhookLogDefault(generics.ListCreateAPIView):

    queryset = VotoWebhookLog.objects.all()
    model = VotoWebhookLog
    serializer_class = VotoWebhookLogSerializer

class VotoWebhookLogInstance(generics.RetrieveUpdateAPIView):

    queryset = VotoWebhookLog.objects.all()
    model = VotoWebhookLog
    serializer_class = VotoWebhookLogSerializer
    lookup_field = 'id'
