%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%%
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_ets).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


-include("common_macros.hrl").
%% --------------------------------------------------------------------
%-record(dns,{service_id,ip_addr,port,vm,timestamp}).
-define(MASTER_ETS,master_ets).

% app_spec: {ServiceId,NumInstances,[RequiredNodes]}.
% catalog_info: {{service,ServiceId},{Type,Source}}.
% node_config: {NodeId,Node,IpAddr,Port,Mode}.
% desired_services: {ServiceId,IpAddr,Port}


%% External exports
%-export([init/0,add/4,delete/3,delete/4,delete/5,
%	 clear/0,get/1,
%	 delete_expired/0,expired/0,
%	 all/0,
%	 get_expired_time/0
%	]).

-compile(export_all).




%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
init()->
    ?MASTER_ETS=ets:new(?MASTER_ETS,[public,set,named_table]),
    ok.
delete_ets()->
    ets:delete(?MASTER_ETS).

clear()->
    true=ets:delete(?MASTER_ETS),
    ?MASTER_ETS=ets:new(?MASTER_ETS,[public,set,named_table]),
    ok.


all()->
    ets:tab2list(?MASTER_ETS).


%% --------------------------------------------------------------------
%% Function: _
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
add(Key,Value)->
    ets:insert(?MASTER_ETS,{Key,Value}),
    ok.


delete(Key)->
    ets:match_delete(?MASTER_ETS,{Key,'_'}),
    ok.

all(Key)->
    case ets:match(?MASTER_ETS,{Key,'$1'}) of
	[]->
	    [];
	Info ->
	    [[Value]]=Info,
	    Value
		
    end.

%%------------- app_spec ----------------------------------------------

add_apps(AppInfo)->
    add(apps,AppInfo).

update_apps(AppInfo)->
     add(apps,AppInfo).

delete_apps()->
    add(apps,[]).

get_apps(WantedServiceId)->
    [{ServiceId,NumInstances,ListRequiredNodes}||
	{ServiceId,NumInstances,ListRequiredNodes}<-all(apps),
	WantedServiceId=:=ServiceId].


%%------------- desired ----------------------------------------------
add_desired(DesiredServices)->
    add(desired_services,DesiredServices).

update_desired(DesiredServices)->
     add(desired_services,DesiredServices).

delete_desired()->
    add(desired_services,[]).

get_desired(WantedServiceId)->
    [{IpAddr,Port}||
	{ServiceId,IpAddr,Port}<-all(desired_services),
	WantedServiceId=:=ServiceId].


%%------------- catalog ----------------------------------------------
add_catalog(CatalogInfo)->
    add(catalog,CatalogInfo).

update_catalog(CatalogInfo)->
     add(catalog,CatalogInfo).

delete_catalog()->
    add(catalog,[]).

get_catalog(WantedServiceId)->
    [{{service,ServiceId},{Type,Source}}||
	{{service,ServiceId},{Type,Source}}<-all(catalog),
	WantedServiceId=:=ServiceId].


%%------------- node ----------------------------------------------
add_nodes(NodesInfo)->
    add(nodes,NodesInfo).

update_nodes(NodesInfo)->
     add(nodes,NodesInfo).

delete_nodes()->
    add(nodes,[]).

get_nodes(WantedNodeId)->
    L=[{NodeId,Node,IpAddr,Port,Mode}||
	{NodeId,Node,IpAddr,Port,Mode}<-all(nodes),
	WantedNodeId=:=NodeId].

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
