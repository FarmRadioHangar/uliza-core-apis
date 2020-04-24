from api_core.settings import TELEGRAM_WEBHOOK_SITE,TELEGRAM_WEBHOOK_PREFIX,TELEGRAM_TOKEN

BOTS = []
for token in TELEGRAM_TOKEN:
    BOTS.append({'TOKEN':token})

#settings.py
DJANGO_TELEGRAMBOT = {

    'MODE' : 'WEBHOOK', #(Optional [str]) # The default value is WEBHOOK,
                        # otherwise you may use 'POLLING'
                        # NB: if use polling mode you must provide to run
                        # a management command that starts a worker

    'WEBHOOK_SITE' : TELEGRAM_WEBHOOK_SITE,
	'WEBHOOK_PREFIX' : TELEGRAM_WEBHOOK_PREFIX, # (Optional[str]) # If this value is specified,
                                  # a prefix is added to webhook url

	#'WEBHOOK_CERTIFICATE' : 'cert.pem', # If your site use self-signed
	                 #certificate, must be set with location of your public key
	                 #certificate.(More info at https://core.telegram.org/bots/self-signed )

	'BOTS' : BOTS,

}
