from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Comment
from log_app.serializers import CommentSerializer

import django_filters
from rest_framework import filters

class CommentGet(generics.ListCreateAPIView):

    queryset = Comment.objects.all()
    model = Comment
    serializer_class = CommentSerializer
    filter_fields = ['id','log','content','contact']

class CommentEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Comment.objects.all()
    model = Comment
    serializer_class = CommentSerializer
    lookup_field = 'id'
