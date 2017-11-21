from django.core.management.base import BaseCommand, CommandError
from log_app.models import Country,Program,Administrator
 
"""
 Put this in crontab to activate hourly checks for telerivet service 
 0 * * * * /opt/virtual-env-27/bin/python /var/www/html/fri-log/fri-log/manage.py schedule_summary
"""
class Command(BaseCommand):

    def handle(self, *args, **options):
        import datetime
        from django.utils import timezone
        countries = Country.objects.all()
        zonetime = timezone.now()   


        for country in countries:
            # to be set for each countries timezone
            hours = datetime.timedelta(hours = 3)
            today = zonetime + hours

            programs = Program.objects.filter(project__end_date__gte=today,confirmed_program_time=True,project__start_date__lte=today,end_date__gte=today,radio_station__country=country)
            today_programs = []
            activated_services = []
            print '---------------------------'
            print country.name

            for program in programs:
                if program.start_date.weekday() == today.weekday() and program.start_date.strftime('%H') == today.strftime('%H'):
                    week = program.weeks - program.weeks_left + 1

                    project_code = program.radio_station.telerivet_project_code

                    if project_code:
                        today_programs.append({'program':program,'project_code':project_code,'week':week,'confirmed':program.confirmed_program_time})
                else:
                    continue

            for program in today_programs:

                import telerivet

                tr = telerivet.API('44IMOUSPZdJPIUitfSWQO22vrfoakSno')
                project = tr.initProjectById(program['project_code'])

                cursor = project.queryServices(
                    context = "message",
                    sort_dir = 'desc'
                )
                # re.match('pulse week 1(?:\D+|$)',s)
                regexp = '(\D|$).*$'
                import re
                for service in cursor.limit(60):
                    program_services = str(program['program'].name) + ' week '
                    service_match = str(program_services)+ str(program['week'])+regexp
                    match = re.match(service_match,service.name)
                    if(match):
                        service.active = True
                        activated_services.append(service.name)                        
                    elif(service.name.startswith(program_services) and service.active == True):
                        service.active = False                        
                    service.save()

            print 'Activated services - ' + str(activated_services)

            to = Administrator.objects.filter(country=country,user__is_staff = True).values_list('user__email',flat=True)
            if not (activated_services == [] or to == []):
                from django.template import Context
                from django.template.loader import render_to_string, get_template
                from django.core.mail import EmailMessage

                subject = today.strftime('%A, %d/%m/%y')+" - Hourly Telerivet Service Activation"
                from_email = 'FRI-LOG <fri4logsheets@gmail.com>'
                message = '<div style="font-size:15px;"><b>'+today.strftime('%H')+':00 Activated services</b> <br><br>'
                count = 1
                for service in activated_services:
                    message = message+' &nbsp;('+str(count)+') '+str(service)+'<br><br>'
                    count = count+1
                message = message + '</div>'

                msg = EmailMessage(subject, message, to=to, from_email=from_email)
                msg.content_subtype = 'html'
                msg.send()