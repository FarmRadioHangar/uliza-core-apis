from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Review
from log_app.serializers import ReviewSerializer
import django_filters
from rest_framework import filters

class ReviewGet(generics.ListCreateAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    filter_fields = ['id', 'log', 'reviewer']

class ReviewEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    lookup_field = 'id'
