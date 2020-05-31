%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(apps_test).  
   
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

%% --------------------------------------------------------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    check_status(),
    find_missing(),
    find_obsolite(),
   % desired_state(),
    
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
