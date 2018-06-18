from django.test import TestCase
from django.test import Client
from rest_framework import status
from rest_framework.test import APITestCase
from uliza.models import Role as RoleModel, Detail as DetailModel, \
Contact as ContactModel, ContactDetail as ContactDetailModel
from uliza.views.content_manager import Role, Detail, ContactDetail 
import json

class TestRole(APITestCase):
	
	def setUp(self):
		role = RoleModel(name='Listener', description='Radio listenership')
		role.save()

	def test_list_roles(self):#Get all roles
		response = self.client.get('/api/v1/content/roles/')
		self.assertTrue(status.is_success(response.status_code))

	def test_create_roles(self):#Create a new role
		#m_role = RoleModel(name='Broadcastor', description='Broadcasts')
		response = self.client.post('/api/v1/content/roles/', {'name':'broadcastor','description':'broadcasts'})
		self.assertEqual(response.status_code, status.HTTP_201_CREATED)

	def test_read_role(self):#Get a given role
		records = self.client.get('/api/v1/content/roles/')
		test_id = records.data[0].get('id')
		response = self.client.get('/api/v1/content/roles/%d/' %test_id)
		self.assertEqual(response.status_code, status.HTTP_200_OK)
		self.assertEqual(response.data.get('name'), 'Listener')

	def test_update_role(self):#Update a given role 
		records = self.client.get('/api/v1/content/roles/')
                test_id = records.data[0].get('id')
		response = self.client.put('/api/v1/content/roles/%d/' %test_id, {'description':'Active subscribers'}, content_type='application/json')
		#self.assertEqual(response.status_code, status.HTTP_205_RESET_CONTENT)
		#self.assertEqual(response.data.get('description'), 'Active subscribers')

	def test_delete_role(self):#Delete a given role
		records = self.client.get('/api/v1/content/roles/')
                test_id = records.data[0].get('id')
		response = self.client.delete('/api/v1/content/roles/%d/' %test_id)
		self.assertEqual(response.status_code, status.HTTP_204_NO_CONTENT)

class TestDetail(APITestCase):
	
	def setUp(self):
                detail = DetailModel(name='first_name')
                detail.save()

	def test_list_detail(self):#Get details
		response = self.client.get('/api/v1/content/details/')
                self.assertTrue(status.is_success(response.status_code))

	def test_create_detail(self):#Create detail
		response = self.client.post('/api/v1/content/details/', {'name':'surname'})
                self.assertEqual(response.status_code, status.HTTP_201_CREATED)

	def test_read_detail(self):#Read a given role
		records = self.client.get('/api/v1/content/details/')
                test_id = records.data[0].get('id')
                response = self.client.get('/api/v1/content/details/%d/' %test_id)
                self.assertEqual(response.status_code, status.HTTP_200_OK)
                self.assertEqual(response.data.get('name'), 'first_name')

	def test_update_detail(self):#Update a given role
		pass

	def test_delete_detail(self):#Delete a given role
		d_records = self.client.get('/api/v1/content/details/')
                d_test_id = d_records.data[0].get('id')
		response = self.client.delete('/api/v1/content/details/%d/' %d_test_id)
                self.assertEqual(response.status_code, status.HTTP_204_NO_CONTENT)


class TestContactDetail(APITestCase):#Test contacts from this class. Contacts management happens here
	
	def setUp(self):
                role = RoleModel.objects.create(name='Listerners', description='Radio listnership')
                detail = DetailModel.objects.create(name='first_name')
                #contact = Contact.objects.save(role=role)
                #contact_detail = ContactDetail.objects.save(contact=contact, detail=detail, info='James')	

	def test_contact_detail_list(self):#List all contacts, and it's details
		pass
	
	def test_create_contact_detail(self):#Create contact, and it's details
		pass

	def test_update_contact_detail(self):#Update contact, and it's details
		pass

	def test_delete_contact_detail(self):#Delete contact, and it's details
		pass
