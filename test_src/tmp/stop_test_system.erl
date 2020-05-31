%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% {"pod_master",'pod_master@asus',"localhost",40000,parallell}.
%%% {"pod_landet_1",'pod_landet_1@asus',"localhost",50100,parallell}.
%%% {"pod_lgh_1",'pod_lgh_1@asus',"localhost",40100,parallell}.
%%% {"pod_lgh_2",'pod_lgh_2@asus',"localhost",40200,parallell}.
%%%
%%% {"adder_service",2,["pod_landet_1","pod_lgh_2"]}.
%%% {"divi_service",1,[]}.
%%% 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(stop_test_system).  
   
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
    {ok,NodesInfo}=file:consult(?NODE_CONFIG),

    WorkerList=[{NodeId,Node,IpAddr,Port,Mode}||{NodeId,Node,IpAddr,Port,Mode}<-NodesInfo,
						NodeId=/="pod_master"],
    [rpc:call(Node,init,stop,[])||{_NodeId,Node,_IpAddr,_Port,_Mode}<-WorkerList],
    [os:cmd("rm -r "++NodeId)||{NodeId,_Node,_IpAddr,_Port,_Mode}<-WorkerList],
  
    ok.




%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------


