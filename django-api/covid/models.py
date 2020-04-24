from django.db import models

class ChatUser(models.Model):
    user_id = models.CharField(max_length=100,unique=True)
    full_name = models.CharField(max_length=25,null=True,blank=True)
    radio_station = models.CharField(max_length=25,null=True,blank=True)
    country = models.CharField(max_length=10,null=True,blank=True)
    language = models.CharField(max_length=6,null=True,blank=True)
    state = models.IntegerField(null=True, blank=True, default=-1)

    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)

    def __unicode__(self):
        return self.user_id

class Content(models.Model):
    title = models.CharField(max_length=100)
    topic_en = models.CharField(max_length=100,null=True,blank=True)
    topic_fr = models.CharField(max_length=100,null=True,blank=True)
    topic_am = models.CharField(max_length=100,null=True,blank=True)
    content_en = models.TextField()
    content_fr = models.TextField()
    content_am = models.TextField()

    def __unicode__(self):
        return self.topic_en

class Question(models.Model):
    chat_user = models.ForeignKey('ChatUser',null=True,blank=True)
    type = models.CharField(max_length=15,null=True,blank=True)
    content = models.TextField()

    answered_by = models.CharField(max_length=30,null=True,blank=True)
    answer = models.TextField(null=True,blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    last_updated_at = models.DateTimeField(auto_now=True)
    def __unicode__(self):
        return self.content
