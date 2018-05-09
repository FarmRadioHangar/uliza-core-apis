from django.test import TestCase
from django.test import Client
from rest_framework import status
import json
import re
from django.core.urlresolvers import reverse
from rest_framework import status
from rest_framework.test import APIClient, APITestCase 
from django.test import RequestFactory
from uliza.models import Answer

class TestAnswers(TestCase):

	def setUp(self):
		self.client = APIClient()
		data = {'zammad_id':2, 'subscriber_phone':'0784743682', 'audio':'audio213', 'articles_count':2, 'state_id':1}
		self.client.post('/api/v1/answers/questions/', data, format='json')
	
	def test_list_answer(self):
		response = self.client.get('answers')
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		self.assertEqual(Answer.objects.get().sip_username, '')

	def test_create_answer(self):
		#Ensure we can create an answer
		data = {'zammad_id':1, 'subscriber_phone':'0784745682', 'audio':'audio123', 'articles_count':3, 'state_id':1}
		response = self.client.post('/api/v1/answers/questions/', data, format='json')
		self.assertEqual(response.status_code, status.HTTP_201_CREATED)
		self.assertEqual(Answer.objects.count(), 2)
			
	def test_read_answers(self):
		response = self.client.get('/api/v1/answers/questions/')
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		self.assertEqual(Answer.objects.get().id, 7)
		
	def test_read_answer(self):
		#Testing read
		response = self.client.get('/api/v1/answers/questions/7/')
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		
	def test_update_answer(self):
		#Testing put
		put_data = {'zammad_id':3, 'subscriber_phone':'0784743691', 'audio':'audio23', 'articles_count':3, 'state_id':2}
		response = self.client.put('/api/v1/answers/questions/7/', put_data)
		self.assertEqual(Answer.objects.get().zammad_id, 3)
		self.assertEqual(Answer.objects.get().subscriber_phone, '0784743691')
		self.assertEqual(Answer.objects.get().audio, 'audio23')
		self.assertEqual(Answer.objects.get().article_count, 3)
		self.assertEqual(Answer.objects.get().state_id, 2)

	def test_partial_update_answer(self):
		#Testing update
		response = self.client.patch('/api/v1/answers/questions/7/',{'article_count':6})
		self.assertEqual(Answer.objects.get().articles_count, 6)

	def test_delete_answer(self):
		#Testing delete
		response = self.client.get('/api/v1/answers/questions/7/')
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

