%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(create_configs). 
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").
-include("test_conf.hrl").
%% --------------------------------------------------------------------

-compile(export_all).

%% ====================================================================
%% External functions
%% ====================================================================

% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------


% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start()->
    misc_lib:unconsult("app.spec",?app_spec),
    ?assertEqual({ok,?app_spec},file:consult("app.spec")),
    
    misc_lib:unconsult("catalog.info",?catalog_info),
    ?assertEqual({ok,?catalog_info},file:consult("catalog.info")),
    
    misc_lib:unconsult("node.config",?node_config),
    ?assertEqual({ok,?node_config},file:consult("node.config")),
    
   ok.    
   
