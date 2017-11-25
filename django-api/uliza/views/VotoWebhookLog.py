from rest_framework import generics
from uliza.models import VotoWebhookLog
from uliza.serializers import VotoWebhookLogSerializer


class VotoWebhookLogCollection(generics.ListCreateAPIView):

    queryset = VotoWebhookLog.objects.all()
    model = VotoWebhookLog
    serializer_class = VotoWebhookLogSerializer


class VotoWebhookLogInstance(generics.RetrieveUpdateAPIView):

    queryset = VotoWebhookLog.objects.all()
    model = VotoWebhookLog
    serializer_class = VotoWebhookLogSerializer
    lookup_field = 'id'
