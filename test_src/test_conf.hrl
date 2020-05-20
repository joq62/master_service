-define(app_spec,[{"master_service",1,["pod_master"]},{"dns_service",1,["pod_master"]},
			  {"log_service",1,["pod_master"]},{"adder_service",2,["pod_landet_1","pod_lgh_2"]},
			  {"divi_service",1,[]}
			 ]).



-define(node_config,[{"pod_master",'pod_master@asus',"localhost",40000,parallell},
		      {"pod_landet_1",'pod_landet_1@asus',"localhost",50100,parallell},
		      {"pod_lgh_1",'pod_lgh_1@asus',"localhost",40100,parallell},
		      {"pod_lgh_2",'pod_lgh_2@asus',"localhost",40200,parallell}]).


-define(catalog_info,[{{service,"adder_service"},{dir,"/home/pi/erlang/basic"}},
		      {{service,"divi_service"},{dir,"/home/pi/erlang/basic"}},
		      {{service,"boot_service"},{dir,"/home/pi/erlang/basic"}},
		      {{service,"dns_service"},{dir,"/home/pi/erlang/basic"}},
		      {{service,"log_service"},{dir,"/home/pi/erlang/basic"}},
		      {{service,"lib_service"},{dir,"/home/pi/erlang/basic"}}]).
