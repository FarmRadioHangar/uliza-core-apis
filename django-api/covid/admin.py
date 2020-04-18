from django.contrib import admin
from covid.models import Content
# Register your models here.

class ContentAdmin(admin.ModelAdmin):
    pass

admin.site.register(Content, ContentAdmin)
