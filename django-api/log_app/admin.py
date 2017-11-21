from django.contrib import admin
from log_app.models import Contact,Auth0User

admin.site.register(Auth0User)
admin.site.register(Contact)