from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Review,Log
from log_app.serializers import ReviewSerializer
import django_filters
from django_filters.rest_framework import DjangoFilterBackend
from rest_framework import filters

class ReviewGet(generics.ListCreateAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    filter_backends = (filters.OrderingFilter, DjangoFilterBackend,)
    ordering_fields = ('id','created_at')
    filter_fields = ['id', 'log', 'reviewer']

    def get_queryset(self):
        program = self.request.GET.get('program')
        projects = self.request.GET.get('projects')
        if program:
            logs = Log.objects.filter(program=program).values('id')
            queryset = Review.objects.filter(log__in=logs)
        elif projects:
            pk_list = projects.split(',')
            logs = Log.objects.filter(program__project__in=pk_list).values('id')
            queryset = Review.objects.filter(log__in=logs)
        else:
            queryset = Review.objects.all()

        return queryset


class ReviewEntity(generics.RetrieveUpdateDestroyAPIView):
    queryset = Review.objects.all()
    model = Review
    serializer_class = ReviewSerializer
    lookup_field = 'id'
