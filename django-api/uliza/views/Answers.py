from rest_framework import generics, status
from django.http import Http404
from rest_framework.response import Response
from rest_framework.views import APIView
from uliza.models import Answer
from uliza.serializers import AnswerSerializer


class Answers(generics.ListCreateAPIView):
	queryset = Answer.objects.all()
	serializer_class = AnswerSerializer
	paginate_by = 20

	def list(self, request):
		queryset = self.get_queryset()
		serializer = AnswerSerializer(queryset, many=True)
		return Response(serializer.data)



class AnswersInstance(generics.RetrieveUpdateDestroyAPIView):
	queryset = Answer.objects.all()
	serializer_class = AnswerSerializer
	lookup_field = 'id'
	
	
	'''
		

	def partial_update(self, request, *args, **kwargs):
		kwargs['PATCH'] = True
		return self.update(request, *args, **kwargs)


	def update(self, request, *args, **kwargs):
		partial = kwargs.pop('PATCH', False)
		self.object = self.get_object_or_none()
		serializer = self.get_serializer(self.object, data=request.data, partial=partial)
		if not serializer.is_valid():
			try:
				self.pre_save(serializer.object)
			except ValidationError as err:
				return Response(err.message_dict, status=HTTP_404_BAD_REQUEST)

		if self.object is None:
			self.object = serializer.save(force_insert=True)
			self.post_save(self.object, created=True)
			return Response(serializer.data, status=status.HTTP_201_CREATED)

		self.object = serializer.save(force_update=True)
		self.post_save(self.object, created=False)
		return Response(serializer.data, status=status.HTTP_200_OK)
		
	'''
