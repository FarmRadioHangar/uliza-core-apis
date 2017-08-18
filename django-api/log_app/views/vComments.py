from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Comment
from log_app.serializers import CommentSerializer

class CommentGet(generics.ListCreateAPIView):

    queryset = Comment.objects.all()
    model = Comment
    serializer_class = CommentSerializer
    filter_fields = ['id']

class CommentEntity(generics.RetrieveUpdateAPIView):

    queryset = Comment.objects.all()
    model = Comment
    serializer_class = CommentSerializer
    lookup_field = 'id'