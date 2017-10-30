from django.contrib.auth.models import User
from django.template import Context
from django.template.loader import get_template
from django.core.mail import EmailMessage

from_email = 'FRI-LOG <postmaster@mail.uliza.fm>'

subject = "FRI Log - domain name change notice"
template = 'emails/new_domain_notification.html'

users = User.objects.all()[3:]

from django.core.mail.message import make_msgid
from smtplib import SMTPRecipientsRefused
msgid = make_msgid()

for user in users:
	try:
		print user.email
		message = get_template(template).render(Context({"first_name":user.first_name,'username':user.username}))
		msg = EmailMessage(subject, message, bcc=[user.email], from_email=from_email,headers = {'Message-Id':msgid,'Reply-To':'jtesfaye@farmradioet.org'})
		msg.content_subtype = 'html'
		msg.send()
	except SMTPRecipientsRefused:
		print 'error: SMTPRecipientsRefused '+user.username
	

print 'done'