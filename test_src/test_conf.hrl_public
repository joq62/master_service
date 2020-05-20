-define(app_spec,[{"master_service",1,["pod_master"]},{"dns_service",1,["pod_master"]},
			  {"log_service",1,["pod_master"]},{"adder_service",2,["pod_landet_1","pod_lgh_2"]},
			  {"divi_service",1,[]}
			 ]).



-define(node_config,[{"pod_master",'pod_master@asus',"joqhome.dynamic-dns.net",40000,parallell},
		      {"pod_landet_1",'pod_landet_1@asus',"joqhome.dynamic-dns.net",50100,parallell},
		      {"pod_lgh_1",'pod_lgh_1@asus',"joqhome.dynamic-dns.net",40100,parallell},
		      {"pod_lgh_2",'pod_lgh_2@asus',"joqhome.dynamic-dns.net",40200,parallell}]).


-define(catalog_info,[{{service,"adder_service"},{git,"https://github.com/joq62/basic.git"}},
		      {{service,"divi_service"},{git,"https://github.com/joq62/basic.git"}},
		      {{service,"dns_service"},{git,"https://github.com/joq62/basic.git"}},
		      {{service,"log_service"},{git,"https://github.com/joq62/basic.git"}},
		      {{service,"lib_service"},{git,"https://github.com/joq62/basic.git"}}]).
