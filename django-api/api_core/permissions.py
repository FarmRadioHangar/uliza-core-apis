from rest_framework.permissions import BasePermission

SAFE_METHODS = ('GET', 'HEAD', 'OPTIONS')

class IsAuthenticatedOrReadOnly(BasePermission):
    """
    The request is authenticated as a user, or is a read-only request.
    """

    def is_authenticated(self,user):
        if not user.is_anonymous():
            if 'gty' in user and user['gty'] == 'client_credentials':
                return True
        else:
            return False

    def has_permission(self, request, view):
        return (
            request._request.method in SAFE_METHODS or
            self.is_authenticated(request.user)
        )
