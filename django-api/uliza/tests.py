from django.test import TestCase
from django.test import Client
from rest_framework import status
import json
import re
from django.core.urlresolvers import reverse
from rest_framework import status
from rest_framework.test import APIClient, APITestCase, APIRequestFactory 
from django.test import RequestFactory
from uliza.models import Answer

class TestAnswers(TestCase):

	def setUp(self):
		self.client = APIClient()
		data = {'zammad_id':2, 'subscriber_phone':'0784743682', 'audio':'audio213'}
		self.client.post('/api/v1/answers/questions/', data, format='json')
	
	def test_list_answer(self):
		response = self.client.get('/api/v1/answers/questions/')
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		self.assertEqual(Answer.objects.get().subscriber_phone, '0784743682')

	def test_create_answer(self):
		#Ensure we can create an answer
		data = {'zammad_id':1, 'subscriber_phone':'0784745682', 'audio':'audio123'}
		response = self.client.post('/api/v1/answers/questions/', data, format='json')
		self.assertEqual(response.status_code, status.HTTP_201_CREATED)
		self.assertEqual(Answer.objects.count(), 2)
			
	def test_read_answers(self):
		response = self.client.get('/api/v1/answers/questions/')
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		self.assertEqual(Answer.objects.get().id, 6)
		
	def test_partial_update_answer(self):
		#Testing update
		response = self.client.patch('/api/v1/answers/questions/2/',{'article_count':6})
		self.assertEqual(Answer.objects.get().articles_count, 1)

	def test_delete_answer(self):
		#Testing delete
		response = self.client.get('/api/v1/answers/questions/2/')
		self.assertEqual(response.status_code, status.HTTP_404_NOT_FOUND)
		
		

class GetParticipantsTests(TestCase):

    fixtures = ['participants.json']

    def test_get_participants_by_pk(self):
        client = Client()
        response = client.get('/api/v1/participants/1')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertTrue(re.compile(r'json').search(response['Content-Type']))
        self.assertEqual(response.data.get('phone_number'), '255678647268')

        #import pdb; pdb.set_trace()

