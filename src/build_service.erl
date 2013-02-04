-module(build_service).
-export([call/5]).

call(Method, Slug, Hdrs, Body, ConnOpts) ->
	BuildHost = proplists:get_value(build_host,
			ConnOpts, "build.erlangonxen.org:8080"),
	UserName = proplists:get_value(username, ConnOpts),
	Password = proplists:get_value(password, ConnOpts),

	Encoded = base64:encode_to_string(lists:append([UserName,":",Password])),
	AuthHeader = {"Authorization","Basic " ++ Encoded},
	Headers = [AuthHeader] ++ Hdrs,

	Location = "https://" ++ BuildHost ++ "/1" ++ Slug,
	Request = case Body of
		none ->
			{Location,Headers};
		{CT,BodyData} ->
			{Location,Headers,CT,BodyData}
		end,

	case httpc:request(Method, Request, [{timeout,infinity}], []) of
	{ok,{{_,200,_},_,RespBody}} -> 
		{ok,RespBody};

	{ok,{{_,204,_},_,_}} ->
		ok;

	{ok,{{_,403,_},_,_}} ->
		forbidden;

	{ok,{{_,404,_},_,_}} ->
		not_found;

	Other ->
		rebar_log:log(error, "~p", [Other]),
		Other
	end.

%%EOF
