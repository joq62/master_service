%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
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
%-export([init/0,delete_ets/0,clear/0,all/0]).




%% ====================================================================
%% External functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
%% @doc: init creates a ets table (public, set with the name master_ets)
-spec(init()->ok).
init()->
    ?MASTER_ETS=ets:new(?MASTER_ETS,[public,set,named_table]),
    ok.

%% @doc: delete_ets deletes the ets tabel with the name master_ets
-spec(delete_ets()->ok).
delete_ets()->
    ets:delete(?MASTER_ETS).

%% @doc: clear deletes current table and creates a new with
-spec(clear()->ok).
clear()->
    true=ets:delete(?MASTER_ETS),
    ?MASTER_ETS=ets:new(?MASTER_ETS,[public,set,named_table]),
    ok.

%% @doc: all returns all tuples in the the table master_ets
-spec(all()->[tuple()]|[]).
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
    [{ServiceId,RequiredNode}||
	{ServiceId,RequiredNode}<-all(apps),
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
    [{ServiceId,Type,Source}||
	{ServiceId,Type,Source}<-all(catalog),
	WantedServiceId=:=ServiceId].


%%------------- node ----------------------------------------------
add_nodes(NodesInfo)->
    add(nodes,NodesInfo).

update_nodes(NodesInfo)->
     add(nodes,NodesInfo).

delete_nodes()->
    add(nodes,[]).

get_nodes(WantedNodeId)->
    L=[{NodeId,IpAddr,Port,Mode}||
	{NodeId,IpAddr,Port,Mode}<-all(nodes),
	WantedNodeId=:=NodeId],
    L.

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
