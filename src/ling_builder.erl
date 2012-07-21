-module(ling_builder).

-export(['ling-build'/2,'ling-image'/2]).

'ling-build'(Config, _AppFile) ->

	case whereis(ling_queue) of
	undefined ->
		ling_queue:start_link();
	_ ->
		ok
	end,

	BaseDir = rebar_config:get_global(base_dir, undefined),
	IsBaseDir = rebar_utils:get_cwd() =:= BaseDir,

	IsPluginDir = filename:basename(rebar_utils:get_cwd()) =:= "ling_builder",

	if not IsPluginDir ->

		Files = [filename:absname(F) || F <- filelib:wildcard("ebin/*")],
		ling_queue:add(Files);

	true ->
		skip
	end,

	if IsBaseDir ->

		ProjName = filename:basename(BaseDir),
		LingOpts = rebar_config:get(Config, ling_opts, []),
		ling_queue:start_build(ProjName, LingOpts);

	true ->
		skip
	end,

	ok.

'ling-image'(Config, _AppFile) ->

	BaseDir = rebar_config:get_global(base_dir, undefined),
	IsBaseDir = rebar_utils:get_cwd() =:= BaseDir,

	if not IsBaseDir ->
		ok;
	true ->

		rebar_log:log(info, "starting inets~n", []),
		application:start(crypto),
		application:start(public_key),
		application:start(ssl),
		application:start(inets),

		ProjName = filename:basename(BaseDir),
		LingOpts = rebar_config:get(Config, ling_opts, []),

		{_,BuildHost} = lists:keyfind(build_host, 1, LingOpts),
		{_,UserName} = lists:keyfind(username, 1, LingOpts),
		{_,Password} = lists:keyfind(password, 1, LingOpts),

		Encoded = base64:encode_to_string(lists:append([UserName,":",Password])),
		AuthHeader = {"Authorization","Basic " ++ Encoded},
		Headers = [AuthHeader],

		%%ImageFile = "vmling-" ++ ProjName,
		ImageFile = "vmling",

		Location = "https://" ++ BuildHost ++ "/1/build/" ++ ProjName,
		case httpc:request(get, {Location,Headers}, [], []) of
		{ok,{{_,200,_},_,RespBody}} ->
			ImageBin = list_to_binary(RespBody),
			rebar_log:log(info, "saving image to ~s [~w byte(s)]~n",
									[ImageFile,byte_size(ImageBin)]),
			ok = file:write_file(ImageFile, ImageBin),
			io:format("LBS: image saved to ~s~n", [ImageFile]);
		{ok,{{_,403,_},_,_}} ->
			io:format("LBS: image is not available yet~n", []);
		{ok,{{_,404,_},_,_}} ->
			io:format("LBS: image is not (yet) available~n", [])
		end,

		ok
	end.

%%EOF
