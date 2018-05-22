from rest_framework import generics, status
from django.http import Http404
from rest_framework.response import Response
from rest_framework.views import APIView
from uliza.models import Answer
from uliza.serializers import AnswerSerializer
from rest_framework.mixins import UpdateModelMixin


class Answers(generics.ListCreateAPIView):
	queryset = Answer.objects.all()
	serializer_class = AnswerSerializer
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


class AnswersInstance(generics.RetrieveUpdateDestroyAPIView, UpdateModelMixin):
	queryset = Answer.objects.all()
	serializer_class = AnswerSerializer
	lookup_field = 'id'

	def get(self, request, *args, **kwargs):
		return self.retrieve(request, *args, **kwargs)

	def put(self, request, *args, **kwargs):
		return self.update(request, *args, **kwargs)
	
	
	def patch(self, request, *args, **kwargs):
		return self.partial_update(request, *args, **kwargs)

	def update(self, request, *args, **kwargs):
		partial = kwargs.pop('partial', False)
		instance = self.get_object()
		serializer = self.get_serializer(instance, data=request.data, partial=partial)
		serializer.is_valid(raise_exception=True)
		self.perform_update(serializer)
		return Response(serializer.data)
		

