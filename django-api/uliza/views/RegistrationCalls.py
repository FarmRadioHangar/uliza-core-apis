from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from uliza.models import RegistrationCall
from uliza.serializers import RegistrationCallSerializer

class RegistrationCalls(generics.ListCreateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer

class RegistrationCallsEntity(generics.RetrieveUpdateAPIView):

    queryset = RegistrationCall.objects.all()
    model = RegistrationCall
    serializer_class = RegistrationCallSerializer
    lookup_field = 'id'


#from django.http import JsonResponse
#from rest_framework import status
#from rest_framework.decorators import api_view
#from rest_framework.exceptions import NotFound
#from uliza.models import RegistrationCall
#from uliza.serializers import RegistrationCallSerializer
#
#@api_view(['POST', 'GET'])
#def default(request):
#
#    if request.method == 'GET': 
#
#        registrationCalls = RegistrationCall.objects.all()
#        serializer = RegistrationCallSerializer(registrationCalls, many=True)
#
#        return JsonResponse(serializer.data, safe=False)
#
#    elif request.method == 'POST': 
#
#        serializer = RegistrationCallSerializer(data=request.data)
#
#        if serializer.is_valid():
#
#            serializer.save()
#
#            return JsonResponse(serializer.data, status=status.HTTP_201_CREATED)
#
#        return JsonResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
#
#@api_view(['PUT', 'PATCH', 'GET'])
#def instance(request, id):
#
#    try:
#        registrationCall = RegistrationCall.objects.get(pk=id)
#    except RegistrationCall.DoesNotExist:
#        raise NotFound
#
#    if request.method == 'GET': 
#        
#        serializer = RegistrationCallSerializer(registrationCall)
#
#        return JsonResponse(serializer.data)
#
#    elif request.method == 'PUT' or request.method == 'PATCH': 
#
#        if request.method == 'PATCH':
#            data = registrationCall.__dict__.copy()
#            data.update(request.data)
#        else:
#            data = request.data
#
#        serializer = RegistrationCallSerializer(registrationCall, data=data)
#
#        if serializer.is_valid():
#
#            serializer.save()
#
#            return JsonResponse(serializer.data)
#
#        return JsonResponse(serializer.errors, 
#                            status=status.HTTP_400_BAD_REQUEST)
#
#    elif request.method == 'DELETE': 
#        
#        pass 
#
#    return False
