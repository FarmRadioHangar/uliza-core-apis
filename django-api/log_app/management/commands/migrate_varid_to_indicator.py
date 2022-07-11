from log_app.models import Indicator,Target

indicators = Indicator.objects.all()

for indicator in indicators:
	target_variable_id = 'target_'+str(indicator.grouping+1)+'_'+str(indicator.order)
	targets =  Target.objects.filter(variable_identifier = target_variable_id)

	for target in targets:
		print('processing',target.id)
		target.indicator = indicator
		target.save()

print('done')
