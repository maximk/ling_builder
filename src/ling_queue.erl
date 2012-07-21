-module(ling_queue).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([add/1]).
-export([start_build/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Files) ->
	gen_server:call(?SERVER, {add,Files}).

start_build(ProjName, LingOpts) ->
	gen_server:call(?SERVER, {start_build,ProjName,LingOpts}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok, []}.

handle_call({add,More}, _From, Files) ->
    {reply, ok, More ++ Files};

handle_call({start_build,ProjName,LingOpts}, _From, Files) ->

	rebar_log:log(info, "starting inets~n", []),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(inets),

	AH = {"Accept","text/plain"},
	case call_lbs(get, "/projects/" ++ ProjName, [AH], none, LingOpts) of
	{ok,Body} ->
		RemoteFiles = string:tokens(Body, "|"),

		FileNames = [filename:basename(F) || F <- Files],
		DeleteUs = lists:filter(fun(F) ->
				not lists:member(F, FileNames)
			end, RemoteFiles),

		lists:foreach(fun(F) ->
				rebar_log:log(info, "delete stale ~s~n", [F]),
				Slug = "/projects/" ++ ProjName ++ "/" ++ F,
				ok = call_lbs(delete, Slug, [], none, LingOpts)
			end, DeleteUs);
	
	not_found ->
		ok = call_lbs(put, "/projects/" ++ ProjName, [], [], LingOpts)
	end,

	lists:foreach(fun(F) ->
		FileName = filename:basename(F),
		{ok,Bin} = file:read_file(F),
		rebar_log:log(info, "upload ~s [~w byte(s)]~n",
								[FileName,byte_size(Bin)]),
		Slug = "/projects/" ++ ProjName ++ "/" ++ FileName,
		ok = call_lbs(put, Slug, [], Bin, LingOpts)
	end, Files),

	rebar_log:log(info, "build started for '~s'~n", [ProjName]),
	{ok,Banner} = call_lbs(post, "/build/" ++ ProjName, [], [], LingOpts),
	io:format("LBS: ~s~n", [Banner]),
	
	{reply,ok,[]}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

call_lbs(Method, Slug, Hdrs, Body, LingOpts) ->
	{_,BuildHost} = lists:keyfind(build_host, 1, LingOpts),
	{_,UserName} = lists:keyfind(username, 1, LingOpts),
	{_,Password} = lists:keyfind(password, 1, LingOpts),

	Encoded = base64:encode_to_string(lists:append([UserName,":",Password])),
	AuthHeader = {"Authorization","Basic " ++ Encoded},
	Headers = [AuthHeader] ++ Hdrs,

	Location = "https://" ++ BuildHost ++ "/1" ++ Slug,
	Request = if Body =:= none ->
			{Location,Headers};
		true ->
			{Location,Headers,"application/octet-stream",Body}
		end,

	case httpc:request(Method, Request, [], []) of
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
