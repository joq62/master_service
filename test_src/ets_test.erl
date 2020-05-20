%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ets_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").

%% --------------------------------------------------------------------
-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================

%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    lib_ets:init(),
    {ok,NodesInfo}=file:consult(?NODE_CONFIG),
    ok=lib_ets:add_nodes(NodesInfo),
    {ok,AppInfo}=file:consult(?APP_SPEC),
    ok=lib_ets:add_apps(AppInfo),
    {ok,CatalogInfo}=file:consult(?CATALOG_INFO),
    ok=lib_ets:add_catalog(CatalogInfo),
    DesiredServices=lib_master:create_service_list(AppInfo,NodesInfo),
    ok=lib_ets:add_desired(DesiredServices),
    check_apps(),   
    check_nodes(),
    check_catalog(),
    check_desired(),
    lib_ets:delete_ets(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_desired()->
    ?assertMatch([{"divi_service","localhost",50100},
		  {"master_service","localhost",40000},
		  {"dns_service","localhost",40000},
		  {"log_service","localhost",40000},
		  {"adder_service","localhost",50100},
		  {"adder_service","localhost",40200}],lib_ets:all(desired_services)),
    ?assertMatch([{"localhost",50100},{"localhost",40200}],
		 lib_ets:get_desired("adder_service")),

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_catalog()->
    ?assertMatch([{{service,"adder_service"},
		   {dir,"/home/pi/erlang/basic"}},
		  {{service,"divi_service"},{dir,"/home/pi/erlang/basic"}},
		  {{service,"boot_service"},{dir,"/home/pi/erlang/basic"}},
		  {{service,"dns_service"},{dir,"/home/pi/erlang/basic"}},
		  {{service,"log_service"},{dir,"/home/pi/erlang/basic"}},
		  {{service,"lib_service"},{dir,_}}],lib_ets:all(catalog)),
    ?assertMatch([{{service,"adder_service"},{dir,"/home/pi/erlang/basic"}}],
		 lib_ets:get_catalog("adder_service")),
   ?assertMatch([],
		lib_ets:get_catalog("glurk")),

    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_nodes()->
    ?assertMatch([{"pod_master",pod_master@asus,"localhost",40000,
                       parallell},
                      {"pod_landet_1",pod_landet_1@asus,"localhost",50100,
                       parallell},
                      {"pod_lgh_1",pod_lgh_1@asus,"localhost",40100,parallell},
                      {"pod_lgh_2",pod_lgh_2@asus,"localhost",40200,
                       parallell}],lib_ets:all(nodes)),
    ?assertMatch([{"pod_master",pod_master@asus,"localhost",40000,parallell}],
		 lib_ets:get_nodes("pod_master")),

    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_apps()->
    ?assertMatch([{"master_service",1,["pod_master"]},
		  {"dns_service",1,["pod_master"]},
		  {"log_service",1,["pod_master"]},
		  {"adder_service",2,["pod_landet_1","pod_lgh_2"]},
		  {"divi_service",1,[]}],lib_ets:all(apps)),
    ?assertMatch([{"adder_service",2,["pod_landet_1","pod_lgh_2"]}],
		 lib_ets:get_apps("adder_service")),

    ok.
    



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
find_obsolite()->
  %  ?assertMatch([],tcp_client:call({"localhost",45000},{list_to_atom("glurk_service"),ping,[]})),
    DS=master_service:desired_services(),
    ?assertMatch([],lib_master:check_obsolite_services(DS)),
    DS2=[{"divi_service","localhost",40000}|DS],
    ?assertMatch([],lib_master:check_obsolite_services(DS2)),
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
find_missing()->
  %  ?assertMatch([],tcp_client:call({"localhost",45000},{list_to_atom("glurk_service"),ping,[]})),
    DS=master_service:desired_services(),
    ?assertMatch([{"divi_service","localhost",_40000},
		  {"adder_service","localhost",_50100},
		  {"adder_service","localhost",_40200}],lib_master:check_missing_services(DS)),
    DS2=[{"master_service","localhost",40000}|DS],
    ?assertMatch([{"divi_service","localhost",_40000},
		  {"adder_service","localhost",_50100},
		  {"adder_service","localhost",_40200}],lib_master:check_missing_services(DS2)),
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
check_status()->
        AppsInfo=master_service:apps(),
    NodesInfo=master_service:nodes(),
    ?assertMatch([{"master_service",1,["pod_master"]},
		  {"dns_service",1,["pod_master"]},
		  {"adder_service",2,["pod_landet_1","pod_lgh_2"]},
		  {"divi_service",1,[]}],AppsInfo),
						%  ok,
    ?assertEqual({"adder_service",2,["pod_landet_1","pod_lgh_2"]},
		 lists:keyfind("adder_service",1,AppsInfo)),
    ?assertEqual(false,
		 lists:keyfind("glurk",1,AppsInfo)),
    
    DesiredServiceList=lib_master:create_service_list(AppsInfo,NodesInfo),

    ?assertMatch([{"divi_service","localhost",50100},
		   {"master_service","localhost",40000},
		   {"dns_service","localhost",40000},
		   {"adder_service","localhost",50100},
		   {"adder_service","localhost",40200}],DesiredServiceList),
		 
    ?assertMatch([{"adder_service","localhost",_50100},
		  {"adder_service","localhost",_40200}],
		 lib_master:service_node_info("adder_service",DesiredServiceList)),
    ?assertMatch([{"divi_service","localhost",_40000}],
		 lib_master:service_node_info("divi_service",DesiredServiceList)),
    ?assertEqual([],
		 lib_master:service_node_info("glurk_service",DesiredServiceList)),
    ok.
