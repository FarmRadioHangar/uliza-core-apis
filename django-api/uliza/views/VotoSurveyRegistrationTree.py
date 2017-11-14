from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from uliza.models import VotoSurveyRegistrationTree
from uliza.serializers import VotoSurveyRegistrationTreeSerializer


class VotoSurveyRegistrationTreeDefault(generics.ListCreateAPIView):

    queryset = VotoSurveyRegistrationTree.objects.all()
    model = VotoSurveyRegistrationTree
    serializer_class = VotoSurveyRegistrationTreeSerializer


class VotoSurveyRegistrationTreeInstance(generics.RetrieveUpdateAPIView):

    queryset = VotoSurveyRegistrationTree.objects.all()
    model = VotoSurveyRegistrationTree
    serializer_class = VotoSurveyRegistrationTreeSerializer
    lookup_field = 'survey_id'
