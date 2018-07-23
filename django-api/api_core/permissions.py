import jwt
from django.utils.encoding import smart_text
from django.utils.translation import ugettext as _
from rest_framework import exceptions
from rest_framework.authentication import (
    BaseAuthentication, get_authorization_header
)
from api_core.settings import JWT_AUTH
from rest_framework.permissions import BasePermission

SAFE_METHODS = ('GET', 'HEAD', 'OPTIONS')

class CheckAuth0AccessGrant(BasePermission):
    def has_permission(self, request, view):
        verified_payload = self.verify_token(request)
        print request._request.method in SAFE_METHODS or verified_payload

        return (
            request._request.method in SAFE_METHODS or
            verified_payload
        )

    def verify_token(self, request):

        jwt_value = self.get_jwt_value(request)
        if jwt_value is None:
            return None

        try:
            payload = jwt.decode(jwt_value,JWT_AUTH['JWT_PUBLIC_KEY'],
                                 issuer=JWT_AUTH['JWT_ISSUER'],
                                 audience=JWT_AUTH['JWT_AUDIENCE'],
                                 verify=True)

        except jwt.ExpiredSignature:
            msg = _('Signature has expired.')
            raise exceptions.AuthenticationFailed(msg)
        except jwt.DecodeError:
            msg = _('Error decoding signature.')
            raise exceptions.AuthenticationFailed(msg)
        except jwt.InvalidTokenError:
            raise exceptions.AuthenticationFailed()

        if not payload['gty'] == 'client-credentials':
            return False

        return True

    def get_jwt_value(self, request):
        auth = get_authorization_header(request).split()
        auth_header_prefix = JWT_AUTH['JWT_AUTH_HEADER_PREFIX'].lower()

        if auth == []:
            return None
        else:
            if smart_text(auth[0].lower()) != auth_header_prefix:
                return None

            return auth[1]

    def authenticate_header(self, request):
        return '{0} realm="{1}"'.format(JWT_AUTH['JWT_AUTH_HEADER_PREFIX'], self.www_authenticate_realm)
