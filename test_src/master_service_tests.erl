%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : 
%%% Three computers 
%%% {"pod_computer_1", "localhost",40100,parallell, 40101, 10}
%%% {"pod_computer_2", "localhost" 40200,parallell, 40201, 10}
%%% {"pod_computer_3", "localhost" 40300,parallell, 40301,10}
%%% Each pod has its port number as vm name pod_40101@asus
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(master_service_tests). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").

%% --------------------------------------------------------------------

%% External exports
%-export([start/0]).
-compile(export_all).


%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
cases_test()->

    ?debugMsg("Start stop_test_system:start"),
    stop_test_system:start(),
    ?debugMsg("Start clean_start"),
    clean_start(),
    
    ?debugMsg("Start ets test"),
    ets_test:start(),
    
    ?debugMsg("Start start_test_system:start"),
    start_test_system:start(),
 
   ?debugMsg("Start orchistrate_test:start"),
    orchistrate_test:start(),
    ?debugMsg("Start stop_test_system:start"),
    stop_test_system:start(),
   % catalog_test:start(),

%   app_test_cases:start(),
     
     
  %   node_controller_test_cases:start(),
   %  app_controller_test_cases:start(),
  %   master_service_test_cases:
  %   master_service_test_cases:
   %  system_test_cases:test_adder_divi(),
     % cleanup and stop eunit 
     stop_computer_pods(),
     clean_stop(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,2*60,master_service}) end).

clean_start()->
    create_configs:start(),
    os:cmd("rm -rf  latest.log"),
    {ok,NodesInfo}=file:consult("node.config"),
    L1=lists:keydelete(node(),2, NodesInfo),
    [rpc:call(Vm,init,stop,[])||{_,Vm,_,_}<-L1],
    [pod:delete(VmName)||{VmName,_,_,_}<-L1],
    ok.


stop_computer_pods()->
    {ok,NodesInfo}=file:consult("node.config"),
    L1=lists:keydelete(node(),2, NodesInfo),
    [rpc:call(Vm,init,stop,[])||{_,Vm,_,_}<-L1],
    [pod:delete(VmName)||{VmName,_,_,_}<-L1],
    ok.

clean_stop()->
    stop_service(lib_service),
    stop_service(log_service),
    stop_service(dns_service),
   
    os:cmd("rm -rf pod_master lib_service log_service logfiles dns_service include"),
    timer:sleep(1000),
    init:stop(),
    ok.



%% --------------------------------------------------------------------
%% Function:support functions
%% Description: Stop eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------

start_service(Service)->
    ?assertEqual(ok,application:start(Service)).
check_started_service(Service)->
    ?assertMatch({pong,_,Service},Service:ping()).
stop_service(Service)->
    ?assertEqual(ok,application:stop(Service)),
    ?assertEqual(ok,application:unload(Service)).

