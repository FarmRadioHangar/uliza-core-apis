from django.contrib import admin
from log_app.models import Contact,Auth0User,Program

admin.site.register(Auth0User)
admin.site.register(Contact)
admin.site.register(Program)
