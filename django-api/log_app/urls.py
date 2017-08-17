from django.conf.urls import patterns,include,url

radiostations = patterns('log_app.views.vRadiostations',
	# url(r'/view/(?P<id>\d+)','view',name="view"),
	# url(r'/add','add',name="add"),
	# url(r'/edit/(?P<id>\d+)','edit',name="edit"),
	url(r'$','index',name='index'),	
)



# Main URL Patterns
urlpatterns = patterns('',
	#  Modules
	url(r'radio_stations',include(radiostations,'radiostations')),
)