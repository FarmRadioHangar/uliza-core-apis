from django.http import JsonResponse
from rest_framework import status
from rest_framework.decorators import api_view
from rest_framework.exceptions import NotFound
from uliza.models import VotoResponseData
from uliza.serializers import VotoResponseDataSerializer

@api_view(['POST', 'GET'])
def default(request):

    if request.method == 'GET': 

        responseData = VotoResponseData.objects.all()
        serializer = VotoResponseDataSerializer(responseData, many=True)

        return JsonResponse(serializer.data, safe=False)

    elif request.method == 'POST': 
        
        serializer = VotoResponseDataSerializer(data=request.data)

        if serializer.is_valid():

            serializer.save()

            return JsonResponse(serializer.data, status=status.HTTP_201_CREATED)

        return JsonResponse(serializer.errors, status=status.HTTP_400_BAD_REQUEST)


@api_view(['PUT', 'PATCH', 'GET'])
def instance(request, id):

    try:
        responseData = VotoResponseData.objects.get(pk=id)
    except VotoResponseData.DoesNotExist:
        raise NotFound

    if request.method == 'GET': 
        
        serializer = VotoResponseDataSerializer(responseData)

        return JsonResponse(serializer.data)

    elif request.method == 'PUT' or request.method == 'PATCH': 

        if request.method == 'PATCH':
            data = responseData.__dict__.copy()
            data.update(request.data)
        else:
            data = request.data

        serializer = VotoResponseDataSerializer(responseData, data=data)

        if serializer.is_valid():

            serializer.save()

            return JsonResponse(serializer.data)

        return JsonResponse(serializer.errors, 
                            status=status.HTTP_400_BAD_REQUEST)

    elif request.method == 'DELETE': 
        
        pass 

    return False
