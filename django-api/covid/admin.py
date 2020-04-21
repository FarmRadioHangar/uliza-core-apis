from django.contrib import admin
from covid.models import *
# Register your models here.

class ContentAdmin(admin.ModelAdmin):
    pass

class ChatUserAdmin(admin.ModelAdmin):
    pass

class QuestionAdmin(admin.ModelAdmin):
    pass

admin.site.register(Question, QuestionAdmin)
admin.site.register(ChatUser, ChatUserAdmin)
admin.site.register(Content, ContentAdmin)
