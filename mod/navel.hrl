%%
%%
%%

-record(navconf, {
	handlers = [],
	port = 9090,
	www_root = ".",
	mod_path = "xbin"
}).

-record(navreq, {
	sock,
	method,
	what,
	params,
	vsn,
	headers,
	body
}).

%% EOF
