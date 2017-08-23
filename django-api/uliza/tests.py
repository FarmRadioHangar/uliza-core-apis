from django.test import TestCase
from django.test import Client
from rest_framework import status
import json
import re

class GetParticipantsTests(TestCase):

    fixtures = ['participants.json']

    def test_get_participants_by_pk(self):
        client = Client()
        response = client.get('/api/v1/participants/1')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        #import pdb; pdb.set_trace()
        self.assertTrue(re.compile(r'json').search(response._headers['content-type']))
        json_body = json.loads(response.content)
        self.assertEqual(json_body.get('phone_number'), '255678647268')

