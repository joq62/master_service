%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(service_handler).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


-include("common_macros.hrl").
%% --------------------------------------------------------------------

%% External exports
%-export([]).

-compile(export_all).




%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
load_start(ServiceId,IpAddrPod,PortPod) ->
    L=[{NodeId,Node,IpAddrNode,PortNode}
       ||{NodeId,Node,IpAddrNode,PortNode,_Mode}<-lib_ets:all(nodes),
	 {IpAddrNode,PortNode}=:={IpAddrPod,PortPod}],
    
    case L of
	[]->
	    Reply={error,[eexists,IpAddrPod,PortPod,?MODULE,?LINE]};
	L->
	    [{NodeId,_Node,IpAddrNode,PortNode}]=L,
	    case lib_ets:get_catalog(ServiceId) of 
		[]->
		    {error,[eexist," in catalago ",ServiceId]}; 
		CatalogResult->
		    [CatalogInfo]=CatalogResult,
		    Reply={tcp_client:call({IpAddrNode,PortNode},
					   {container,create,[NodeId,[CatalogInfo]]}),
			   ServiceId,IpAddrPod,PortPod},
		    case Reply of
			{ok,ServiceId,IpAddrPod,PortPod}->
			    lib_service:log_event(?MODULE,?LINE,info,
						  ["ok Started ",ServiceId,IpAddrPod,PortPod]),
			    ok=tcp_client:call(?DNS_ADDRESS,{dns_service,add,
							     [ServiceId,IpAddrNode,PortNode]});
			Err->
			    lib_service:log_event(?MODULE,?LINE,info,
						  ["Error Started Service ",ServiceId,IpAddrPod,PortPod,Err])
		    end
	    end
    end.


stop_unload(ServiceId,IpAddrPod,PortPod)->
    [L]=[{NodeId,Node,IpAddrNode,PortNode}
	 ||{NodeId,Node,IpAddrNode,PortNode,_Mode}<-lib_ets:all(nodes),
	   {IpAddrNode,PortNode}=:={IpAddrPod,PortPod}],
    {NodeId,_Node,IpAddrNode,PortNode}=L,
    [ok]=tcp_client:call({IpAddrNode,PortNode},{container,delete,[NodeId,[ServiceId]]}),
    ok=tcp_client:call(?DNS_ADDRESS,{dns_service,delete,[ServiceId,IpAddrNode,PortNode]}),
    lib_service:log_event(?MODULE,?LINE,info,["ok Stopped Service ",ServiceId,IpAddrPod,PortPod]),
    ok.



%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
