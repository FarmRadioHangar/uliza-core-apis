from django.http import HttpResponse
from django.views.generic import View
import base64
import json
import requests
import logging
from uliza.models import Answer
from uliza.views.events_helper import EventsHelper

class Events(View):
	#Logging call status
	def call_state(self, code):
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
		response = requests.post('/update', data=null)
		logging.info('Sent a request Viamo webhook')
		
		EventHelper.assert_body_field(response, 'delivery_status')
		EventHelper.assert_body_field(response, 'outgoing_call_id')
		EventsHelper.assert_query_param(response, 'audio_block_id')
			
		response_data = json.loads(request.body)
		delivery_status = int(response_data.delivery_status)
                outgoing_call_id = response_data.outgoing_call_id
                status_message = ''
		if delivery_status is 7:
 
			#Process call if not None
			if (outgoing_call_id is not None):
				message_block = get_block(response_data.interactions, block_id)
				#Checks for availability of audio from the call before processing it
				if (message_block.response is  None and message_block.response.open_audio_url is  None):
					raise 'Could not find audio response block matching ID '+block_id
				#Encode audio file
				encoded_audio = base64.b64encode(message_block.response.open_audi_url)
				logs = response_data.data.delivery_logs
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
				#Received audio is then sent to zammad for ticket creation
				zammad_response = Events.create_ticket(json.dumps(Payload))
				#Save the ticket to uliza database
				zammad_data = json.loads(response.body)
				Answer.objects.create(zammad_id=zammad_data.id, subscriber_phone=phone, audio=encoded_audio)			
				
		#Check for closed tickets in zammad and send answers to viamo

	
			
		return HttpResponse(status=200)

	def post(self, request, *args, **kwargs):
		
		return HttpResponse('/v1/api/events/')

	def put(self, request, *args, **kwargs):
		#return make_request(viamo, uri, options, 'PUT', data)
		return HttpResponse('/v1/api/events')

	def patch(self, request, *args, **kwargs):
		#return make_request(viamo, uri, options, 'PATCH', data)
		return HttpResponse('/v1/api/events')
