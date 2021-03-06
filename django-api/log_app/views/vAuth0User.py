from django.http import JsonResponse
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import Auth0User,Contact,RadioStation
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
        raise Http404

    # Check password
    if check_password(data['password'],user.password):
        contact = Contact.objects.filter(user_id='local|'+str(user.id))

        if not contact:
            try:
                contact = RadioStation.objects.get(group_account_id='local|'+str(user.id))
            except RadioStation.DoesNotExist:
                raise Http404

            name = contact.name
            app_metadata = {'role': 'group',
                            'is_superuser': False,
                            'is_admin': False}
            contact.group_account_id='auth0|'+str(user.id)
        else:
            contact = contact[0]
            name = contact.first_name+' '+contact.last_name
            app_metadata = {'role': contact.role,
                            'is_superuser': contact.is_superuser,
                            'is_admin': contact.is_admin}

            contact.user_id = 'auth0|'+str(user.id)
        contact.save()

        user = {'user_id':user.id,
                'username':user.username,
                'name':name,
                'email':user.email,
                'app_metadata': app_metadata
                }


        user = json.dumps(user)

        return HttpResponse(user,content_type = 'application/javascript; charset=utf8')
    else:
        return Http404('User does not exist')


@require_POST
def check_user_by_email(request):
    import json
    data = json.loads(request.body)
    try:
        user = Auth0User.objects.get(email=data['email'])
    except Auth0User.ValueError:
        raise Http404
    except Auth0User.DoesNotExist:
        raise Http404

    # Check password
    contact = Contact.objects.filter(user_id='local|'+str(user.id))

    if not contact:
        try:
            contact = RadioStation.objects.get(group_account_id='local|'+str(user.id))
        except RadioStation.DoesNotExist:
            raise Http404

        name = contact.name
        app_metadata = {'role': 'group',
                        'is_superuser': False,
                        'is_admin': False}
    else:
        contact = contact[0]
        name = contact.first_name+' '+contact.last_name
        app_metadata = {'role': contact.role,
                        'is_superuser': contact.is_superuser,
                        'is_admin': contact.is_admin}

    user = {'user_id':user.id,
            'username':user.username,
            'name':name,
            'email':user.email,
            'app_metadata': app_metadata
            }

    user = json.dumps(user)

    return HttpResponse(user,content_type = 'application/javascript; charset=utf8')
