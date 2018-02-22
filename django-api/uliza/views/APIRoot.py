from rest_framework.views import APIView
from rest_framework.response import Response


class APIRoot(APIView):
    """
    Uliza API v1
    """

    def get(self, request):
        return Response({'message': 'OK'})
