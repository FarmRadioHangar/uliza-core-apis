from rest_framework import generics
from log_app.models import Notification
from log_app.serializers import NotificationSerializer

class NotificationGet(generics.ListAPIView):
    queryset = Notification.objects.all()
    model = Notification
    serializer_class = NotificationSerializer
    filter_fields = ['id','sent_to','seen']


class NotificationEntity(generics.UpdateAPIView):
    queryset = Notification.objects.all()
    model = Notification
    serializer_class = NotificationSerializer
    lookup_field = 'id'
