%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(master_service_test_cases). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").

%% --------------------------------------------------------------------
-compile(export_all).


-define(MASTER_IP,{"localhost",40000}).

%% ====================================================================
%% External functions
%% ====================================================================

start_init_test()->
    ok=application:start(log_service),
    true=dns_service:add("log_service","localhost",40000,pod_master@asus),
    ok=application:start(master_service),
    true=dns_service:add("master_service","localhost",40000,pod_master@asus),
    
    ?assertMatch([{"dns_service","localhost",40000,pod_master@asus,
		   _1584312498},
		  {"log_service","localhost",40000,pod_master@asus,
		   _1584312498},
		  {"master_service","localhost",40000,pod_master@asus,
		   _1584312498}],dns_service:all()),
    ?assertMatch([{"divi_service","localhost",50100},
		  {"master_service","localhost",40000},
		  {"dns_service","localhost",40000},
		  {"log_service","localhost",40000},
		  {"adder_service","localhost",50100},
		  {"adder_service","localhost",40200}],master_service:desired_services()),
    ok.
