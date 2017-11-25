from rest_framework import generics
from uliza.models import VotoSurveyRegistrationTree
from uliza.serializers import VotoSurveyRegistrationTreeSerializer


class VotoSurveyRegistrationTreeCollection(generics.ListCreateAPIView):

    queryset = VotoSurveyRegistrationTree.objects.all()
    model = VotoSurveyRegistrationTree
    serializer_class = VotoSurveyRegistrationTreeSerializer


class VotoSurveyRegistrationTreeInstance(generics.RetrieveUpdateAPIView):

    queryset = VotoSurveyRegistrationTree.objects.all()
    model = VotoSurveyRegistrationTree
    serializer_class = VotoSurveyRegistrationTreeSerializer
    lookup_field = 'voto_survey_id'
