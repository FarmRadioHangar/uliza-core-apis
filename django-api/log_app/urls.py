from django.conf.urls import patterns,include,url
from django.views.generic import TemplateView

radiostations = patterns('log_app.views.vRadiostations',
	# url(r'/view/(?P<id>\d+)','view',name="view"),
	# url(r'/add','add',name="add"),
	# url(r'/edit/(?P<id>\d+)','edit',name="edit"),
	url(r'$','stations_list',name='stations_list'),	
)

# Main URL Patterns
urlpatterns = patterns('',
	#  Modules
	url(r'radiostations',include(radiostations,'radiostations')),
)