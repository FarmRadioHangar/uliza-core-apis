from django.db import models
from log_app.models import Country

# Create your models here.
from django.db import models
from log_app.models import Country

class ChatUser(models.Model):
    chat_id = models.CharField(max_length=100)
    country = models.ForeignKey(Country,null=True,blank=True)
    language = models.CharField(max_length=6)

    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)

class Content(models.Model):
    topic_en = models.CharField(max_length=100)
    topic_fr = models.CharField(max_length=100)
    topic_am = models.CharField(max_length=100)
    content_en = models.TextField()
    content_fr = models.TextField()
    content_am = models.TextField()

    def __unicode__(self):
        return self.topic_en

class Question(models.Model):
    chat_id = models.ForeignKey('ChatUser')
    content = models.TextField()
