%% This is the application resource file (.app file) for the 'base'
%% application.
{application, master_service,
[{description, "master_service  " },
{vsn, "0.0.95" },
{modules, 
	  [master_service_app,master_service_sup,master_service,
	   misc_lib,pod,container]},
{registered,[master_service]},
{applications, [kernel,stdlib]},
{mod, {master_service_app,[]}},
{start_phases, []}
]}.
