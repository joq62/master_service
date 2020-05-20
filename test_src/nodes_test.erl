%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nodes_test).  
   
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
    find_missing(), 
    available(),
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
available()->
    NodesInfo=master_service:nodes(),
    ?assertMatch([{"pod_master",pod_master@asus,"localhost",40000,parallell}],
		 lib_master:check_available_nodes(NodesInfo)),
  
    
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
find_missing()->
  %  ?assertMatch([],tcp_client:call({"localhost",45000},{list_to_atom("glurk_service"),ping,[]})),
    NodesInfo=master_service:nodes(),
    ?assertMatch([{"pod_landet_1",pod_landet_1@asus,"localhost",50100,parallell},
		  {"pod_lgh_1",pod_lgh_1@asus,"localhost",40100,parallell},
		  {"pod_lgh_2",pod_lgh_2@asus,"localhost",40200,parallell}],
		 lib_master:check_missing_nodes(NodesInfo)),
  
    
    ok.

