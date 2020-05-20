%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(orchistrater).  
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common_macros.hrl").
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------


-compile(export_all).

%-export([]).


%% ====================================================================
%% External functions
%% ====================================================================

-define(APP_INFO_FILE,"app_info.dets").
-define(APP_DETS,?APP_INFO_FILE,[{type,set}]).



%% --------------------------------------------------------------------

%% External exports

%-export([create/2,delete/2]).

-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
campaign()->
     %% campaign
    %% 1). Update configs - ensure that only availble nodes are part of the orchistration
    %% 2). Remove missing services from dns . Registered Service Not memeber of Desired  
    %% 3). Try to start missing services based on available nodes

    %% 1).
    ok=lib_master:update_configs(),
   
    %% 2).
    DesiredServices=lib_ets:all(desired_services),
    case tcp_client:call(?DNS_ADDRESS,{dns_service,all,[]},?CLIENT_TIMEOUT) of
	{error,Err}->
	    {error,Err};
	RegisteredServices->	
	    RemoveDns=[{ServiceId,IpAddr,Port}||{ServiceId,IpAddr,Port,_,_}<-RegisteredServices,
						false==lists:member({ServiceId,IpAddr,Port},DesiredServices)],
	    [tcp_client:call(?DNS_ADDRESS,{dns_service,delete,[ServiceId,IpAddr,Port]},?CLIENT_TIMEOUT)
	     ||{ServiceId,IpAddr,Port}<-RemoveDns],
	    %% 3).
	    lib_master:remove_obsolite(),
	    lib_master:start_missing(),
	    ok
    end.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_available_nodes(NodesInfo)->
    PingR=[{tcp_client:call({IpAddr,Port},{net_adm,ping,[Node]},?CLIENT_TIMEOUT),NodeId,Node,IpAddr,Port,Mode}||{NodeId,Node,IpAddr,Port,Mode}<-NodesInfo],
    ActiveNodes=[{NodeId,Node,IpAddr,Port,Mode}||{pong,NodeId,Node,IpAddr,Port,Mode}<-PingR],
    ActiveNodes.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_missing_nodes(NodesInfo)->
    PingR=[{tcp_client:call({IpAddr,Port},{net_adm,ping,[Node]},?CLIENT_TIMEOUT),NodeId,Node,IpAddr,Port,Mode}||{NodeId,Node,IpAddr,Port,Mode}<-NodesInfo],
    ActiveNodes=[{NodeId,Node,IpAddr,Port,Mode}||{pong,NodeId,Node,IpAddr,Port,Mode}<-PingR],
    Missing=[{DesiredNodeId,DesiredNode,DesiredIpAddr,DesiredPort,DesiredMode}||
		{DesiredNodeId,DesiredNode,DesiredIpAddr,DesiredPort,DesiredMode}<-NodesInfo,
		false=:=lists:member({DesiredNodeId,DesiredNode,DesiredIpAddr,DesiredPort,DesiredMode},ActiveNodes)],
    
    Missing.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_obsolite_services(DesiredServices)->
%{"dns_service","localhost",40000,pod_master@asus,1584047881}
    RegisteredServices=dns_service:all(),
    PingR=[{tcp_client:call({IpAddr,Port},{list_to_atom(ServiceId),ping,[]},?CLIENT_TIMEOUT),IpAddr,Port}||{ServiceId,IpAddr,Port,_,_}<-RegisteredServices],
    ActiveServices=[{atom_to_list(ServiceId),IpAddr,Port}||{{pong,_,ServiceId},IpAddr,Port}<-PingR],
    Obsolite=[{ObsoliteServiceId,ObsoliteIpAddr,ObsolitePort}||{ObsoliteServiceId,ObsoliteIpAddr,ObsolitePort}<-ActiveServices,
							   false=:=lists:member({ObsoliteServiceId,ObsoliteIpAddr,ObsolitePort},DesiredServices)],
 
   Obsolite.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_missing_services(DesiredServices)->
    PingR=[{tcp_client:call({IpAddr,Port},{list_to_atom(ServiceId),ping,[]},?CLIENT_TIMEOUT),IpAddr,Port}||{ServiceId,IpAddr,Port}<-DesiredServices],
    ActiveServices=[{atom_to_list(ServiceId),IpAddr,Port}||{{pong,_,ServiceId},IpAddr,Port}<-PingR],
    Missing=[{DesiredServiceId,DesiredIpAddr,DesiredPort}||{DesiredServiceId,DesiredIpAddr,DesiredPort}<-DesiredServices,
							   false=:=lists:member({DesiredServiceId,DesiredIpAddr,DesiredPort},ActiveServices)],
    
    Missing.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_service(ServiceId,NodeDir,Node,CatalogInfo,NodesInfo)->
    {Source,ServiceId,Path}=lists:keyfind(ServiceId,2,CatalogInfo),
    {_NodeId,Node,IpAddr,Port,_Mode}=lists:keyfind(Node,2,NodesInfo),

    ok=container:create(Node,NodeDir,
			[{{service,ServiceId},
			  {Source,Path}}
			]),
    true=dns_service:add(ServiceId,IpAddr,Port,Node),
    ok.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------

service_node_info(Key,ServiceNodeInfo)->
    [{ServiceId,IpAddr,Port}||{ServiceId,IpAddr,Port}<-ServiceNodeInfo,ServiceId=:=Key].

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
create_service_list(AppInfo,NodesInfo)->
    create_service_list(AppInfo,NodesInfo,[]).
create_service_list([],_,ServiceList)->
    ServiceList;

create_service_list([{ServiceId,_Num,[]}|T],NodesInfo,Acc)->

    %% GLURK smarter alogrithm 
    [{_NodeId,_Node,IpAddr,Port,_Mode}|_]=NodesInfo,
    NewAcc=[{ServiceId,IpAddr,Port}|Acc],
    create_service_list(T,NodesInfo,NewAcc);

create_service_list([{ServiceId,_Num,Nodes}|T],NodesInfo,Acc) ->
    L=[extract_ipaddr(ServiceId,NodeId,NodesInfo)||NodeId<-Nodes],
    NewAcc=lists:append(Acc,L),
    create_service_list(T,NodesInfo,NewAcc).

extract_ipaddr(ServiceId,NodeId,NodesInfo)->
    case lists:keyfind(NodeId,1,NodesInfo) of
	false->
	    {ServiceId,glurk,ServiceId};
	{_NodeId,_Node,IpAddr,Port,_Mode}->
	    {ServiceId,IpAddr,Port}	
    end.				     
    

%App_list=[{service_id,ip_addr,port,status}], status=running|not_present|not_loaded
%app_info=[{service_id,num,nodes,source}],  
% nodes=[{ip_addr,port}]|[], num = integer. Can be mix of spefied and unspecified nodes. Ex: num=2, nodes=[{ip_addr_1,port_2}] -> one psecifed and one unspecified

%status_desired_state_apps= ok|missing|remove
%status_desired_state_nodes = ok|missing|remove
%% --------------------------------------------------------------------
%% Function:init 
%% --------------------------------------------------------------------



ping_service([],_,PingResult)->
    PingResult;
ping_service([{_VmName,IpAddr,Port}|T],ServiceId,Acc)->
    R=tcp_client:call({IpAddr,Port},{list_to_atom(ServiceId),ping,[]}),
 %   R={ServiceId,VmName,IpAddr,Port},
    ping_service(T,ServiceId,[R|Acc]).
 
