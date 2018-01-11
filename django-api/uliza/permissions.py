from rest_framework import permissions
from jose import jwt


class Auth0JWTScopePermission(permissions.BasePermission):
    """
    Permission that restricts acccess based on scopes available in the access
    token
    """

    def access_type(self, method):
        if ('GET' == method):
            return 'read'
        elif ('POST' == method):
            return 'create'
        elif ('PUT' == method):
            return 'update'
        elif ('DELETE' == method):
            return 'delete'
        return None

    def has_permission(self, request, view):
        if not hasattr(view, 'jwt_scope_namespace'):
            return True
        access = self.access_type(request.method)
        if access is None:
            return False
        token = request.auth
        unverified_claims = jwt.get_unverified_claims(token)
        token_scopes = unverified_claims["scope"].split()
        scope = access + ':' + view.jwt_scope_namespace
        return scope in token_scopes
