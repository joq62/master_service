%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_service_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").



-ifdef(dir).
-define(CHECK_CATALOG,check_catalog_dir()).
-else.
-define(CHECK_CATALOG,check_catalog_git()).
-endif.


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
    ?debugMsg("init_ets"),
    ?assertEqual(ok,init_ets()),
  %  ?assertEqual(ok,lib_master:update_configs()),
    ?debugMsg("get_nodes_status"),
    ?assertEqual(ok,get_nodes_status()),
    ?debugMsg("get_available_services"),
    ?assertEqual(ok,get_available_services()),
    ?debugMsg("get_missing_services"),
    ?assertEqual(ok,get_missing_services()),

    ?debugMsg("start services"),
    ?assertEqual(ok,start_services()),    
    ?debugMsg("get_available_services 2"),
    ?assertEqual(ok,get_available_services2()),
    ?debugMsg("get_missing_services2"),
    ?assertEqual(ok,get_missing_services2()),

    lib_ets:delete_ets(),
    ok.


init_ets()->
    lib_ets:init(),
    {ok,NodesInfo}=file:consult(?NODE_CONFIG),
    ok=lib_ets:add_nodes(NodesInfo),
    {ok,AppInfo}=file:consult(?APP_SPEC),
    ok=lib_ets:add_apps(AppInfo),
    {ok,CatalogInfo}=file:consult(?CATALOG_INFO),
    ok=lib_ets:add_catalog(CatalogInfo),
  %  DesiredServices=lib_master:create_service_list(AppInfo,NodesInfo),
  %  ok=lib_ets:add_desired(DesiredServices),
    ?assertEqual([{"master_sthlm_1","localhost",40000,parallell},
		  {"worker_varmdoe_1","localhost",50100,parallell},
		  {"worker_sthlm_1","localhost",40100,parallell},
		  {"worker_sthlm_2","localhost",40200,parallell}],
		 lib_ets:all(nodes)),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
get_nodes_status()->
    ?assertMatch({{active,[]},
		  {missing,
		   [{"master_sthlm_1","localhost",40000,parallell},
		    {"worker_varmdoe_1","localhost",50100,parallell},
		    {"worker_sthlm_1","localhost",40100,parallell},
		    {"worker_sthlm_2","localhost",40200,parallell}]}},
		 lib_master:get_nodes_status()),
    ok.

get_available_services()->
    ?assertMatch([glurk],lib_master:get_available_services()),
    ok.

get_missing_services()->
    ?assertMatch([{"master_service","master_sthlm_1"},
                      {"dns_service","master_sthlm_1"},
                      {"log_service","master_sthlm_1"},
                      {"adder_service","worker_varmdoe_1"},
                      {"adder_service","worker_sthlm_1"},
                      {"divi_service","worker_sthlm_2"}],lib_master:get_missing_services()),
    ok.
    
start_services()->
    ?assertEqual(ok,container:create("adder_service",git,"https://github.com/joq62/")),
    ?assertMatch({pong,_,adder_service},adder_service:ping()),
    ?assertEqual(ok,lib_service:start_tcp_server("localhost",40100,parallell)),
    ?assertMatch({pong,_,adder_service},tcp_client:call({"localhost",40100},
							{adder_service,ping,[]},?CLIENT_TIMEOUT)),
    ?assertMatch({pong,_,lib_service},tcp_client:call({"localhost",40100},
							{lib_service,ping,[]},?CLIENT_TIMEOUT)),
    
    ?assertMatch({{active,[{"localhost",40100,parallell}]},
		  {missing,
		   [{"master_sthlm_1","localhost",40000,parallell},
		    {"worker_varmdoe_1","localhost",50100,parallell},
		    {"worker_sthlm_2","localhost",40200,parallell}]}},
		 lib_master:get_nodes_status()),
    ok.
			
get_available_services2()->
    ?assertMatch([],lib_master:get_available_services()),
    ok.

get_missing_services2()->
    ?assertMatch([{"master_service","master_sthlm_1"},
		  {"dns_service","master_sthlm_1"},
		  {"log_service","master_sthlm_1"},
		  {"adder_service","worker_varmdoe_1"},
		  {"divi_service","worker_sthlm_2"}],lib_master:get_missing_services()),
    ok.
    
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
