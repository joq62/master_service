%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
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
-export([start/0]).



%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:tes cases
%% Description: List of test cases 
%% Returns: non
%% --------------------------------------------------------------------
start()->
    spawn(fun()->eunit:test({timeout,10*60,master_service}) end).

cases_test()->
    ?debugMsg("Test system setup"),
    setup(),
    %% Start application tests
    
    ?debugMsg("start lib_service "),
    ?assertEqual(ok,application:start(lib_service)),

    ?debugMsg("ets test"),    
    ?assertEqual(ok,ets_test:start()),
  
    ?debugMsg("lib master  test"),    
    ?assertEqual(ok,lib_master_test:start()),
  
    ?debugMsg("start master_service and lib_service"),    
    ?assertEqual(ok,start_session()),

    ?debugMsg("Start stop_test_system:start"),
    %% End application tests
    cleanup(),
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
setup()->
    ok.

start_session()->
    ?assertEqual(ok,application:start(master_service)),
    ?assertMatch({pong,_,master_service},master_service:ping()),   
    ok.


cleanup()->
    ?assertEqual(ok,application:stop(master_service)),  
    ?assertEqual(ok,application:stop(lib_service)),  
    init:stop().




