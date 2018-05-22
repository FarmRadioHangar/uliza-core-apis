from django.http import HttpResponse
from django.views.generic import View
import base64
import json
import requests
from uliza.models import Answer

VIAMO_API_KEY = '' #configured
VIAMO_API_URL = 'https://go.votomobile.org/api/v1/' #configured
ZAMMAD_API_URL = 'https://answers.uliza.fm/api/v1/'

class Events(View):

        def get_block(self, interactions, id):
                for interaction in interactions:
                        if interaction.block_id == id:
                                return interaction
                        return None

        def assert_body_field(request, field):
		data = json.loads(request.body)
                if not data[field]:
                        print 'Missing field '+field
                        raise 'Webhook request fields must include '+field

	def assert_query_param(request, param):
		data = json.loads(request.query)
		if not data[param]:
			print 'Missing parameter '+param
			raise 'Webhook request paramerter must include '+param
		
	def create_ticket(payload, phone, audio_file, audio_mine_data):
		
		return requests.post('tickets', payload)

	def call_state(code):

                return {

                1: 'Queued',
                2: 'Ringing',
                3: 'In Progress',
                4: 'Re-try',
                5: 'Failed, No Answer',
                6: 'Finished, Complete',
                7: 'Finished, Incomplete',
                8: 'Failed, Insufficient credit',
                9: 'Failed, Network',
                10: 'Failed, Cancelled',
                11: 'Sent',
                12: 'Finished, Voicemail',
                13: 'Failed, Error',
                14: 'Invalid Status Code'

                }.get(code, 'Invalid status code')

	def get(self, request, *args, **kwargs):
		#return make_request(viamo, uri, options, 'GET')
		try:
			self.data = json.loads(request.body)
			#get delivery status
			#get outgoing call Id 
			#Process call if not None
			if (dstatus is not 0 and call_id is not None):#Incase of a redirection
				message_block = get_block(data.interactions, block_id)
				if (message_block.response is  None and message_block.response.open_audio_url is  None):
					raise 'Could not find audio response block matching ID '+block_id
				encoded_audio = base64.b64encode(message_block.response.open_audio_url)
				
				logs = request.body.data.delivery_logs
				deliverylog_entry = logs[0]
				mdict = {mime_type:'mime-type'}
				class Attachment:
                       			 filename = messageBlock.response.open_audio_file
                       			 data = '###'
                       			 mdict[mime_type] = 'audio/mp3'

				class Article:
                               		subject = 'n/a'
                               		body = 'n/a'
                               		attachments = [Attachment]

				class PayLoad:
					title = '[viamoOpenEndedAudio]'
                                        group = 'Bart FM'
                                        customer = 'guess:'+ deliverylog_entry.subscriber.phone + '@uliza.fm'
                                        article = Article

				response = create_ticket(Payload, deliverylog_entry.subcriber.phone, message_block.response.open_file, data)

				Answer.objects.create(zammad_id=response.body.id, subscriber_phone=phone, audio=encoded_audio)
				
				#Call post method to save to the database
				return HttpRedirect('/v1/api/answers/questions/')
									
		except:
			'No Data to process'
		return HttpResponse(status=200)

	def post(self, request, *args, **kwargs):

		assert_body_field(request, 'delivery_status')
		assert_body_field(request, 'out_going_call_id')
		assert_query_param(request, 'audio_block_id')

		self.data = json.loads(request.body)
		self. call_status = call_state() 
		self.delivery_status = int(data.delivery_status)
		self.outgoing_call = data.outgoing_call_id
		self.status_message =  call_state(delivery_status)
		
		#'/update'
		try:
			delivery_status = int(request.body.delivery_status)
			outgoing_call_id = request.body.outgoing_call_id
			status_message = ''

			if delivery_status is 7:#Process call by sending control to the get method.
				return HttpRedirect('/', dstatus=delivery_status, block_id=outgoing_call_id)
		
		except:
			raise	
	
		return HttpResponse('')

	def put(self, request, *args, **kwargs):
		#return make_request(viamo, uri, options, 'PUT', data)
		return HttpResponse('put')

	def patch(self, request, *args, **kwargs):
		#return make_request(viamo, uri, options, 'PATCH', data)
		return HttpResponse('patch')
