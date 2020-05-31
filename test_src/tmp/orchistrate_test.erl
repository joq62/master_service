%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orchistrate_test).  
   
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
    ?assertMatch([{"master_service","localhost",40000},
		  {"log_service","localhost",40000},
		  {"dns_service","localhost",40000}],dns_service:all()),
    orchistrater:campaign(),
    timer:sleep(20*1000),
    ?assertMatch(6,lists:flatlength(dns_service:all())),

    ?assertMatch([{"localhost",_},
		  {"localhost",_}],tcp_client:call(?DNS_ADDRESS,{dns_service,get,["adder_service"]})),

    ?assertMatch([{"localhost",_}],tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]})),

    %% do something
    [{IpAddrDivi,PortDivi}|_]=tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]}),
  %  ?debugMsg(IpAddrDivi++integer_to_list(PortDivi)),
    ?assertEqual(42.0,tcp_client:call({IpAddrDivi,PortDivi},{divi_service,divi,[420,10]})),
    
    % remove divi_service
    service_handler:stop_unload("divi_service",IpAddrDivi,PortDivi),
    ?assertMatch({badrpc,_},tcp_client:call({IpAddrDivi,PortDivi},{divi_service,divi,[420,10]})),
    ?assertEqual([],tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]})),

    %% RESTART divi 
    orchistrater:campaign(),
    timer:sleep(10*1000),
  %% do something
    [{IpAddrDivi2,PortDivi2}|_]=tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]}),
    ?assertEqual(84.0,tcp_client:call({IpAddrDivi2,PortDivi2},{divi_service,divi,[840,10]})),

    %% Node missing 
   
    pod:delete("pod_landet_1"),
    ?assertMatch({error,_},tcp_client:call({IpAddrDivi2,PortDivi2},{divi_service,divi,[840,10]})),

    %% campaign
    %% 1). Update configs - ensure that only availble nodes are part of the orchistration
    %% 2). Remove missing services from dns . Registered Service Not memeber of Desired  
    %% 3). Try to start missing services based on available nodes

    orchistrater:campaign(),
   %  ?assertEqual(glurk,tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]})),
    timer:sleep(5*1000),
    [{IpAddrDivi3,PortDivi3}|_]=tcp_client:call(?DNS_ADDRESS,{dns_service,get,["divi_service"]}),
    ?assertEqual(98.0,tcp_client:call({IpAddrDivi3,PortDivi3},{divi_service,divi,[980,10]})),
    ok.


do_call([],ServiceId,M,F,A,Result,N)->
    timer:sleep(500),
    IpAddrList=tcp_client:call(?DNS_ADDRESS,{dns_service,get,[ServiceId]}),
    do_call(IpAddrList,ServiceId,M,F,A,Result,N+1);
do_call([{IpAddr,Port}|_],ServiceId,M,F,A,Result,N)->
    case tcp_client:call({IpAddr,Port},{M,F,A}) of
	{error,_}->
	    timer:sleep(500),
	    IpAddrList=tcp_client:call(?DNS_ADDRESS,{dns_service,get,[ServiceId]}),
	    do_call(IpAddrList,ServiceId,M,F,A,Result,N+1);
	{badrpc,_}->
	     timer:sleep(500),
	    IpAddrList=tcp_client:call(?DNS_ADDRESS,{dns_service,get,[ServiceId]}),
	    do_call(IpAddrList,ServiceId,M,F,A,Result,N+1);
	_->
	    ?assertEqual(Result,tcp_client:call({IpAddr,Port},{M,F,A}))
    end.

%% --------------------------------------------------------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
% Missing,{"divi_service","localhost",40000},


load_start([],StartResult)->
    StartResult;
load_start([{ServiceId,IpAddrPod,PortPod}|T],Acc)->
    NewAcc=[service_handler:load_start(ServiceId,IpAddrPod,PortPod)|Acc],
    load_start(T,NewAcc).



%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
find_obsolite()->
  %  ?assertMatch([],tcp_client:call({"localhost",45000},{list_to_atom("glurk_service"),ping,[]})),
    DS=lib_ets:all(desired_services),
    ?assertMatch([{"dns_service","localhost",40000},
		  {"master_service","localhost",40000}],lib_master:check_obsolite_services(DS)),
    DS2=[{"divi_service","localhost",40000}|DS],
    ?assertMatch([{"dns_service","localhost",40000},
		  {"master_service","localhost",40000}],lib_master:check_obsolite_services(DS2)),
    
    ok.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
available()->
    NodesInfo=lib_ets:all(nodes),
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
    NodesInfo=lib_ets:all(nodes),
    ?assertMatch([{"pod_landet_1",pod_landet_1@asus,"localhost",50100,parallell},
		  {"pod_lgh_1",pod_lgh_1@asus,"localhost",40100,parallell},
		  {"pod_lgh_2",pod_lgh_2@asus,"localhost",40200,parallell}],
		 lib_master:check_missing_nodes(NodesInfo)),
  
    
    ok.

