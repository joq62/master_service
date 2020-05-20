%%% -------------------------------------------------------------------
%%% Author  : Joq Erlang
%%% Description : test application calc
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_master).  

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

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
update_configs()->
    io:format("~p~n",[{?MODULE,?LINE,update_configs}]),
    {ok,AppInfo}=file:consult(?APP_SPEC),
    ok=lib_ets:add_apps(AppInfo),

    {ok,CatalogInfo}=file:consult(?CATALOG_INFO),
    ok=lib_ets:add_catalog(CatalogInfo),

    {ok,NodesInfo}=file:consult(?NODE_CONFIG),
    AvailableNodesInfo=check_available_nodes(NodesInfo),
    ok=lib_ets:add_nodes(AvailableNodesInfo),
    DesiredServices=lib_master:create_service_list(AppInfo,NodesInfo),
    ok=lib_ets:add_desired(DesiredServices),
    ok.			 


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_missing()->
    AppInfo=lib_ets:all(apps),
    NodesInfo=lib_ets:all(nodes),
    DS=lib_master:create_service_list(AppInfo,NodesInfo),
    case lib_master:check_missing_services(DS) of
	[]->
	    [];
	Missing->
	    io:format("~p~n",[{?MODULE,?LINE,missing ,Missing}]),
	    lib_service:log_event(?MODULE,?LINE,info,["Missing services",Missing]),
	    load_start(Missing,[])
    end. 



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------


load_start([],StartResult)->
    StartResult;
load_start([{ServiceId,IpAddrPod,PortPod}|T],Acc)->
    NewAcc=[service_handler:load_start(ServiceId,IpAddrPod,PortPod)|Acc],
    timer:sleep(200),
    load_start(T,NewAcc).


%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
remove_obsolite()->
    io:format("~p~n",[{?MODULE,?LINE,remove_obsolite}]),
    DesiredServices=lib_ets:all(desired_services),
    case check_obsolite_services(DesiredServices) of
	[]->
	    ok;
	Obsolite ->
	    lib_service:log_event(?MODULE,?LINE,info,["Obsolite services ",Obsolite]),
	    remove(Obsolite,[])	    
    end.
remove([],Acc)->
    Acc;
remove([{ServiceId,IpAddr,Port}|T],Acc)->
    Nodes=lib_ets:all(nodes),
    R=case [NodeId||{NodeId,_,Ip1,P1,_}<-Nodes,
	   {Ip1,P1}=:={IpAddr,Port}] of
	[]->
	    {error,[eexist,IpAddr,Port, ?MODULE,?LINE]};
	[NodeId]->
	    case tcp_client:call({IpAddr,Port},{container,delete,[NodeId,[ServiceId]]},?CLIENT_TIMEOUT) of
		{error,Err}->
		    lib_service:log_event(?MODULE,?LINE,error,[Err]),
		    {error,Err};
		[ok] ->
		    tcp_client:call(?DNS_ADDRESS,{dns_service,delete,[ServiceId,IpAddr,Port]},?CLIENT_TIMEOUT),
		    ok
	    end
      end,
    remove(T,[R|Acc]).
    
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_available_nodes(NodesInfo)->
   % {ok,NodesInfo}=file:consult(?NODE_CONFIG),    
    S=self(),Ref=erlang:make_ref(),
    PidList=[spawn(fun()-> p_check_available_nodes(S,Ref,I) end)||I<-NodesInfo],
    N=length(PidList),
    PingR=gather(N,Ref,[]),
    ActiveNodes=[{NodeId,Node,IpAddr,Port,Mode}||{pong,NodeId,Node,IpAddr,Port,Mode}<-PingR],
    ActiveNodes.

p_check_available_nodes(Parent,Ref,{NodeId,Node,IpAddr,Port,Mode})->
    Ret=tcp_client:call({IpAddr,Port},{net_adm,ping,[Node]},?CLIENT_TIMEOUT),
    Parent!{Ref,{Ret,NodeId,Node,IpAddr,Port,Mode}}.

gather(0,_,Result)->
    Result;
gather(N,Ref,Acc) ->
    receive
	{Ref,Ret}->
	    gather(N-1,Ref,[Ret|Acc])
    end.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
check_missing_nodes(NodesInfo)->
%    {ok,NodesInfo}=file:consult(?NODE_CONFIG),
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
    PingR=[{tcp_client:call({IpAddr,Port},{list_to_atom(ServiceId),ping,[]},?CLIENT_TIMEOUT),IpAddr,Port}||{ServiceId,IpAddr,Port}<-RegisteredServices],
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
    ActiveServices=[{atom_to_list(Service),IpAddr,Port}||{{pong,_,Service},IpAddr,Port}<-PingR],
    %ensure that dns is updated 
    [dns_service:add(ServiceId,IpAddr,Port)||{ServiceId,IpAddr,Port}<-ActiveServices],

    Missing=[{DesiredServiceId,DesiredIpAddr,DesiredPort}||{DesiredServiceId,DesiredIpAddr,DesiredPort}<-DesiredServices,
							   false=:=lists:member({DesiredServiceId,DesiredIpAddr,DesiredPort},ActiveServices)],
    
    Missing.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%%  1) Create a pod PodId on computer ComputeId with IpAddrComp  PortComp
%%  2) Start tcp_server on PodId with IpAddrPod and PortPod 
%%  3) Load and  start service ServiceId on PodId 
%%  4) Check if ServiceId is started with ping 
%%  5) Add ServiceId,IpAddrPod and PortPod in dns_service
%% 
%% Returns: non
%% --------------------------------------------------------------------
%load_start_service(IpAddrPod,PortPod,ServiceId,PodId)->
    
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% 1) Create pod 
%% 2) Load and start lib_service
%% 3) start tcp_server
%% ComputerIpInfo={IpAddrComputer,PortComputer}
%% PodArgs={ParentNode,Dir,IpAddrPod,PortPod,Mode}
%% NeedServices=[CatalogInfo1, CatalogInfo2..]
%% Returns: non
%% --------------------------------------------------------------------
start_pod(ComputerIpInfo,PodArgs,NeedServices)->
 %   D=date(),
 %   R=tcp_client:call({"localhost",40000},{erlang,date,[]}),
 %create pod
    {NodeId,IpAddrPod,PortPod,ModePod}=PodArgs,
    tcp_client:call(ComputerIpInfo,{pod,create,[NodeId]}),
 %   R=tcp_client:call(ComputerIpInfo,{net_adm,ping,[Node]}),

     % load lib_service
    [tcp_client:call(ComputerIpInfo,{container,create,
				     [NodeId,
				      [{{service,ServiceId},
					{Source,Path}}]]})
     ||{{service,ServiceId},{Source,Path}}<-NeedServices],
    
   % timer:sleep(10000),
    tcp_client:call(ComputerIpInfo,{rpc,call,[misc_lib:get_node_by_id(NodeId),
					      lib_service,start_tcp_server,
					      [IpAddrPod,PortPod,ModePod]]}),
    R=case tcp_client:call({IpAddrPod,PortPod},{net_adm,ping,[misc_lib:get_node_by_id(NodeId)]}) of
	pong->
	    ok;
	Err->
	   {error,Err}
      end,
    R.
    
		    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
stop_pod(ComputerIpInfo,NodeId)->
    tcp_client:call(ComputerIpInfo,{pod,delete,[NodeId]}).
    
		    

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
start_service(ServiceId,NodeId,_Node,CatalogInfo,NodesInfo)->
    {{service,_Service},{Source,Path}}=lists:keyfind({service,ServiceId},1,CatalogInfo),
    {NodeId,_Node,IpAddr,Port,_Mode}=lists:keyfind(NodeId,1,NodesInfo),

    ok=container:create(NodeId,
			[{{service,ServiceId},
			  {Source,Path}}
			]),
    true=dns_service:add(ServiceId,IpAddr,Port),
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
create_service_list(AppsInfo,NodesInfo)->
    create_service_list(AppsInfo,NodesInfo,[]).
create_service_list([],_,ServiceList)->
    ServiceList;

create_service_list([{ServiceId,Num,[]}|T],NodesInfo,Acc)->
    
    %% GLURK smarter alogrithm 
    AvailableNodes=lib_ets:all(nodes),
    L=[{NodeId,Node,IpAddr,Port,Mode}||{NodeId,Node,IpAddr,Port,Mode}<-AvailableNodes,
				       NodeId=/=?MASTER_NODEID],
    NewAcc= case L of
		[]->
		    Acc;
		L->
		    %% Extract current desired services
		    %% check if service already deployed on an existing node 
		    %% check if can deploy total amount of services (different nodes)
		    
		    U1=[{ServiceId,IpAddr,Port}||{S1,IpAddr,Port}<-lib_ets:all(desired_services),
							  ServiceId=:=S1],
		    U2=keep_available_nodes(U1,L,[]),
		    NumToUpdate=Num-length(U2),
		    UpdatedList=update(NumToUpdate,L,ServiceId,U2),
		    [UpdatedList|Acc]
	    end,
    create_service_list(T,NodesInfo,NewAcc);


create_service_list([{ServiceId,_Num,Nodes}|T],NodesInfo,Acc) ->
    L=[extract_ipaddr(ServiceId,NodeId,NodesInfo)||NodeId<-Nodes],
    NewAcc=lists:append(Acc,L),
    create_service_list(T,NodesInfo,NewAcc).

extract_ipaddr(ServiceId,NodeId,NodesInfo)->
    case lists:keyfind(NodeId,1,NodesInfo) of
	false->
	    {ServiceId,false,false};
	{_NodeId,_Node,IpAddr,Port,_Mode}->
	    {ServiceId,IpAddr,Port}	
    end.				     

update(0,_AvailableNodes,_ServiceId,UpdatedList)->
    UpdatedList;
update(_,[],_,UpdatedList)->
    UpdatedList;
update(N,[{_NodeId,_Node,IpAddr,Port,_Mode}|T],ServiceId,Acc)->
    NewAcc=case [{IpAddr,Port}||{_ServiceId,IpAddr2,Port2}<-Acc,
				{IpAddr2,Port2}=:={IpAddr,Port}] of
	       []->
		   NewN=N-1,
		   [{ServiceId,IpAddr,Port}|Acc];
	       _->
		   NewN=N,
		   Acc
	   end,
    update(NewN,T,ServiceId,NewAcc).
    


keep_available_nodes([],_,ServiceList)->
    ServiceList;
keep_available_nodes([{ServiceId,IpAddr,Port}|T],AvailableNodes,Acc) ->
    L=[{ServiceId,IpAddr,Port}||{_NodeId,_Node,IpAddr2,Port2,_Mode}<-AvailableNodes,
			      {IpAddr,Port}=:={IpAddr2,Port2}],
    NewAcc=lists:append(L,Acc),
    keep_available_nodes(T,AvailableNodes,NewAcc).    



get_ip_port({ServiceId,IpAddr,Port}, AvailableNodes)->
    L=[{IpAddr,Port}||{_NodeId,_Node,Ip1,P1,_Mode}<-AvailableNodes,
		      {Ip1,P1}=:={IpAddr,Port}],
    case L of
	[]->
	    [{_NodeId,_Node,Ip2,P2,_Mode}|_]=AvailableNodes,
	    {ServiceId,Ip2,P2};
	_ ->
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
    R=tcp_client:call({IpAddr,Port},{list_to_atom(ServiceId),ping,[]},?CLIENT_TIMEOUT),
 %   R={ServiceId,VmName,IpAddr,Port},
    ping_service(T,ServiceId,[R|Acc]).
 
