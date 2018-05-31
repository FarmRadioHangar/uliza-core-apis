import logging
import json
import requests
from collections import namedtuple

class EventsHelper:
	LANGUAGE_ID = 206069	

	#Gets block with given audio
	@classmethod
	def get_block(cls, interactions, block_id):
		for interaction in interactions:
			if interaction.block_id == block_id:
				return interaction
			return None

	#Checks whether request data contains a given field
	@classmethod
	def assert_body_field(cls, request, field):
		data = json.loads(request, object_hook=lambda d: namedtuple('F', d.keys())(*d.values()))
		if not hasattr(data,field):
			 return 'Missing field '+field
		return field

	#Checks whether request data contains a given Parameter
	@classmethod
	def assert_query_param(cls, request, param):
		data = json.loads(request, object_hook=lambda d: namedtuple('P', d.keys())(*d.values()))
		if not hasattr(data,param):
			return 'Missing parameter '+param
		return param
	
	#Creates ticket in zammad
	@classmethod
	def create_ticket(cls, payload):
		return requests.post('tickets', json.dumps(payload))


	#Creates message in viamo
	@classmethod
	def create_message(cls, audio, languageId):
		url = 'message?audio_file[' + languageId + ']=' +audioId
		class MessageData:
			has_voice = 1
			has_sms = 0
			title = 'Uliza Answers Response Message'
	 
		return requests.post(url, json.dumps(MessageData.__dict__))

	#Schedule outgoing call in Viamo
	@classmethod
	def schedule_outgoing_call(cls, messageId, phonenumber):
		data = '{"subscriber_data":"{\\"receiver_voice\\":1, \\"receiver_sms\\":1, \\"contact\\":phonenumber}"}'
		return requests.post('outgoing_calls', json.dumps(data.__dict__))

	#Sends response to question to viamo for an answer to the initial caller
	@classmethod
	def send_answer(cls, audioId, ticket):
		response = create_message(audioId, LANGUAGE_ID)
		raw_data = json.loads(request, object_hook=lambda d:namedtuple('F', d.keys())(*d.values()))
		messageId =  json.loads(raw_data[0].data)
		schedule_outgoing_call(messageId, ticket.subscriber_phone)
