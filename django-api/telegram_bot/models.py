from django.db import models
from log_app.models import Project, Program

class ProgramSubscription(models.Model):
    username = models.CharField(max_length=80)
    chat_id = models.CharField(max_length=100)
    programs = models.ManyToManyField(Program,blank=True,null=True)

    # Time track
    last_updated_at = models.DateTimeField(auto_now=True)
    created_at = models.DateTimeField(auto_now_add=True)
