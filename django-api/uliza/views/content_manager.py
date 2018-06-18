from rest_framework import generics, status
from rest_framework.response import Response
from uliza.models import Role as RoleModel, Detail as DetailModel, ContactDetail as ContactDetailModel
from rest_framework.mixins import UpdateModelMixin
from uliza.serializers import RoleSerializer, DetailSerializer, ContactDetailSerializer

class Role(generics.ListCreateAPIView):
	queryset = RoleModel.objects.all()
	serializer_class = RoleSerializer
	paginate_by = 20

	def list(self, request, *args, **kwargs):
		queryset = self.filter_queryset(self.get_queryset())

		page = self.paginate_queryset(queryset)
		if page is not None:
			serializer = self.get_serializer(page, many=True)
			return self.get_paginated_response(serializer.data)
		
		serializer = self.get_serializer(queryset, many=True)
		return Response(serializer.data)

	def post(self, request, *args, **kwargs):
		return self.create(request, *args, **kwargs)

	def get(self, request, *args, **kwargs):
		return self.list(request, *args, **kwargs)

class RoleInstance(generics.RetrieveUpdateDestroyAPIView, UpdateModelMixin):
	queryset = RoleModel.objects.all()
	serializer_class = RoleSerializer
	lookup_field = 'id'


class Detail(generics.ListCreateAPIView):
	queryset = DetailModel.objects.all()
	serializer_class = DetailSerializer
	paginate_by = 20

class DetailInstance(generics.RetrieveDestroyAPIView, UpdateModelMixin):
	queryset = DetailModel.objects.all()
	serializer_class = DetailSerializer
	lookup_field = 'id'

'''
class Contact(generics.ListCreateAPIView):
	queryset = Detail.objects.all()
	serializer = DetailSerializer

class ContactInstance(generics.RetrieveDestroyAPIView, UpdateModelMixin):
	queryset = Contact.objects.all()
	serializer = ContactSerializer
	lookup_field = 'id'
'''

#Manage contacts from Contact detail
class ContactDetail(generics.ListCreateAPIView):
	queryset = ContactDetailModel.objects.all()
	serializer_class = ContactDetailSerializer
	paginate_by = 20

class ContactDetailInstance(generics.RetrieveDestroyAPIView, UpdateModelMixin):
	queryset = ContactDetailModel.objects.all()
	serializer_class = ContactDetailSerializer
	lookup_field = 'id'

