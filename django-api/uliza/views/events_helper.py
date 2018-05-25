import logging
import json
import requests

class EventsHelper:
	LANGUAGE_ID = 206069	

	#Gets block with given audio
	def get_block(self, interactions, id):
		for interaction in interactions:
			if interaction.block_id == id:
				return interaction
			return None

	#Checks whether request data contains a given field
	def assert_body_field(self, request, field):
		self.data = json.loads(request.body)
		if not data[field]:
			logging.error('Missing field '+field)	

	#Checks whether request data contains a given Parameter
	def assert_query_param(self, request, param):
		self.data = json.loads(request.query)
		if not data[param]:
			logging.error('Missing parameter '+param)
	
	#Creates ticket in zammad
	def create_ticket(self, payload):
		return requests.post('tickets', json.dumps(payload))


	#Creates message in viamo
	def create_message(self, audio, languageId):
		self.url = 'message?audio_file[' + languageId + ']=' +audioId
		class MessageData:
			has_voice = 1
			has_sms = 0
			title = 'Uliza Answers Response Message'
	 
		return requests.post(url, json.dumps(MessageData))

	#Schedule outgoing call in Viamo
	def schedule_outgoing_call(self, messageId, phonenumber):
		self.data = ''
		return requests.post('outgoing_calls', json.dumps(data))

	#Sends response to question to viamo for an answer to the initial caller
	def send_answer(self, audioId, ticket):
		self.response = create_message(audioId, LANGUAGE_ID)
		self.messageId =  json.loads(response.body.data)
		schedule_outgoing_call(messageId, ticket.subscriber_phone)
	
