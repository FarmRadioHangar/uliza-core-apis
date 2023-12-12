from django.http import JsonResponse
from rest_framework.response import Response
from rest_framework import generics
from rest_framework import status
from rest_framework.exceptions import NotFound
from rest_framework.views import APIView
from log_app.models import RadioTransmission,RadioStation,Program
from log_app.serializers import RadioTransmissionSerializer

class RadioTransmissionGet(generics.ListCreateAPIView):

    queryset = RadioTransmission.objects.all()
    model = RadioTransmission
    many = True
    serializer_class = RadioTransmissionSerializer
    ordering_fields = ('id')
    filter_fields = ['id','radio_station','radio_station__country']

    def get_queryset(self):
        project = self.request.GET.get('project')
        pk_list = self.request.GET.get('ids')

        if project:
            stations = Program.objects.filter(project = project).values_list('radio_station',flat=True).distinct()
            queryset = RadioTransmission.objects.filter(radio_station__in=stations)
        elif pk_list:
            pk_list = pk_list.split(',')
            queryset = RadioTransmission.objects.filter(id__in=pk_list)
        else:
            queryset = RadioTransmission.objects.all()

        return queryset

    def create(self, request, *args, **kwargs):
        transmissions = request.data.getlist('frequency')
        index = 0
        data = []

        for transmission in transmissions:
            data.append({'frequency': request.data.getlist('frequency')[index],
    				    'radio_station': request.data.getlist('radio_station')[index],
    				     'gain': request.data.getlist('gain')[index],
    					 'height': request.data.getlist('height')[index],
    				     'power': request.data.getlist('power')[index],
    				     # 'mapping_data': request.data.getlist('mapping_data')[index],
                         'longitude': request.data.getlist('longitude')[index],
                         'latitude': request.data.getlist('latitude')[index]})

            serializer = self.get_serializer(data=data[index])
            serializer.is_valid(raise_exception=True)
            self.perform_create(serializer)
            index = index +1

        headers = self.get_success_headers(data)
        return Response(data, status=status.HTTP_201_CREATED, headers=headers)


class RadioTransmissionEntity(generics.RetrieveUpdateDestroyAPIView):

    queryset = RadioTransmission.objects.all()
    model = RadioTransmission
    serializer_class = RadioTransmissionSerializer
    lookup_field = 'id'

    def destroy(self, request, *args, **kwargs):
        if 'radio_station' in request.GET:
            instances = RadioTransmission.objects.filter(radio_station=request.GET['radio_station'])
            instances.delete()
        else:
            instance = self.get_object()
            self.perform_destroy(instance)

        return Response(status=status.HTTP_204_NO_CONTENT)

    def update(self, request, *args, **kwargs):
        if 'radio_station' in request.data:
            instances = RadioTransmission.objects.filter(radio_station=request.data['radio_station'][0]).exclude(id__in = request.data['id'])
            instances.delete()

            # todo updating radio transmission
            instances = RadioTransmission.objects.filter(id__in = request.data['id'])
            index = 0

            for instance in instances:
                form_data={'frequency': request.data['frequency'][index],
                        'radio_station': request.data['radio_station'][index],
                        'gain': request.data['gain'][index],
                        'height': request.data['height'][index],
                        'mapping_data': request.data['mapping_data'][index],
                        'power': request.data['power'][index],
                        'longitude': request.data['longitude'][index],
                        'latitude': request.data['latitude'][index]},

                serializer = self.get_serializer(instance,data=form_data[0],partial=True)
                serializer.is_valid(raise_exception=True)
                self.perform_update(serializer)
                index = index +1
        else:
            partial = kwargs.pop('partial', False)
            instance = self.get_object()
            serializer = self.get_serializer(instance, data=request.data, partial=partial)

            serializer.is_valid(raise_exception=True)
            self.perform_update(serializer)



        if getattr(instance, '_prefetched_objects_cache', None):
            # If 'prefetch_related' has been applied to a queryset, we need to
            # forcibly invalidate the prefetch cache on the instance.
            instance._prefetched_objects_cache = {}

        return Response(serializer.data)
