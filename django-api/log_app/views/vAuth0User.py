from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Auth0User,Contact
from log_app.serializers import Auth0UserSerializer
from django.http import Http404,HttpResponse
from django.contrib.auth.hashers import check_password
from django.views.decorators.http import require_POST

import django_filters
from rest_framework import filters

class Auth0UserGet(generics.ListCreateAPIView):

    queryset = Auth0User.objects.all()
    model = Auth0User
    serializer_class = Auth0UserSerializer
    filter_fields = ['id','username']

class Auth0UserEntity(generics.RetrieveUpdateAPIView):
    queryset = Auth0User.objects.all()
    model = Auth0User
    serializer_class = Auth0UserSerializer
    lookup_field = 'id'


@require_POST
def authenticate(request):
    import json
    data = json.loads(request.body)
    try:
        user = Auth0User.objects.get(username=data['username'])
    except Auth0User.DoesNotExist:
        return Http404

    # Check password
    if check_password(data['password'],user.password):
        try:
            contact = Contact.objects.get(user_id=user.id)
        except Contact.DoesNotExist:
            raise Http404('User does not exist')

        user = {'user_id':user.id,'username':user.username,'name':contact.first_name+' '+contact.last_name,'email':user.email}

        user = json.dumps(user)

        return HttpResponse(user,content_type = 'application/javascript; charset=utf8')
    else:
        raise Http404('User does not exist')
