from django.conf.urls import patterns, include, url
from log_app.views.vRadiostations import *
from log_app.views.vPrograms import *
from log_app.views.vAdministrators import *
from log_app.views.vComments import *
from log_app.views.vCountries import *
from log_app.views.vGroupAccounts import *
from log_app.views.vKnowledgePartners import *
from log_app.views.vLogs import *
from log_app.views.vPresenters import *
from log_app.views.vProjects import *

radio_stations = patterns('log_app.views.vRadiostations',
    url(r'/(?P<id>\d+)$', RadioStationEntity.as_view()),
    url(r'$', RadioStationGet.as_view()),
)

programs = patterns('log_app.views.vPrograms',
    url(r'/(?P<id>\d+)$', ProgramEntity.as_view()),
    url(r'$', ProgramGet.as_view()),
)

administrators = patterns('log_app.views.vAdministrators',
    url(r'/(?P<id>\d+)$', AdministratorEntity.as_view()),
    url(r'$', AdministratorGet.as_view()),
)

comments = patterns('log_app.views.vComments',
    url(r'/(?P<id>\d+)$', CommentEntity.as_view()),
    url(r'$', CommentGet.as_view()),
)

countries = patterns('log_app.views.vCountries',
    url(r'/(?P<id>\d+)$', CountryEntity.as_view()),
    url(r'$', CountryGet.as_view()),
)

group_accounts = patterns('log_app.views.vGroupAccounts',
    url(r'/(?P<id>\d+)$', GroupAccountEntity.as_view()),
    url(r'$', GroupAccountGet.as_view()),
)

knowledge_partners = patterns('log_app.views.vKnowledgePartners',
    url(r'/(?P<id>\d+)$', KnowledgePartnerEntity.as_view()),
    url(r'$', KnowledgePartnerGet.as_view()),
)

logs = patterns('log_app.views.vLogs',
    url(r'/(?P<id>\d+)$', LogEntity.as_view()),
    url(r'$', LogGet.as_view()),
)

presenters = patterns('log_app.views.vPresnter',
    url(r'/(?P<id>\d+)$', PresenterEntity.as_view()),
    url(r'$', PresenterGet.as_view()),
)

projects = patterns('log_app.views.vProject',
    url(r'/(?P<id>\d+)$', ProjectEntity.as_view()),
    url(r'$', ProjectGet.as_view()),
)

urlpatterns = patterns('',
    url(r'radio_stations', include(radio_stations, 'radio_stations')),
    url(r'programs', include(programs, 'programs')),
    url(r'administrators', include(administrators, 'administrators')),
    url(r'comments', include(comments, 'comments')),
    url(r'countries', include(countries, 'countries')),
    url(r'group_accounts', include(group_accounts, 'group_accounts')),
    url(r'knowledge_partners', include(knowledge_partners, 'knowledge_partners')),
    url(r'logs', include(logs, 'logs')),
    url(r'presenters', include(presenters, 'presenters')),
    url(r'projects', include(projects, 'projects')),
)
