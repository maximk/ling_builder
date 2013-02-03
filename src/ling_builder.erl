-module(ling_builder).

-export(['ling-build'/2,'ling-image'/2,'ling-build-image'/2]).

'ling-build'(Config, _AppFile) ->
	do_ling_build(Config, fun(_ProjName, _LingOpts) -> ok end).

do_ling_build(Config, Continue) ->

	case whereis(ling_queue) of
	undefined ->
		ling_queue:start_link();
	_ ->
		ok
	end,

	%%BaseDir = rebar_config:get_global(base_dir, undefined),
	BaseDir = rebar_config:get_xconf(Config, base_dir),
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
		LingOpts = rebar_config:get(Config, ling_builder_opts, []),
		add_misc_files(LingOpts),
		ling_queue:start_build(ProjName, LingOpts),

		Continue(ProjName, LingOpts);

	true ->
		skip
	end,

	ok.

'ling-image'(Config, _AppFile) ->

	%%BaseDir = rebar_config:get_global(base_dir, undefined),
	BaseDir = rebar_config:get_xconf(Config, base_dir),
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
		LingOpts = rebar_config:get(Config, ling_builder_opts, []),

		retrieve_image(ProjName, LingOpts)
	end.

retrieve_image(ProjName, LingOpts) ->
	case build_service:call(get, "/build/" ++ ProjName ++ "/image",
									[], none, LingOpts) of
	{ok,RespBody} ->

		ImageFile = "vmling",
		ImageBin = list_to_binary(RespBody),
		rebar_log:log(info, "saving image to ~s [~w byte(s)]~n",
								[ImageFile,byte_size(ImageBin)]),
		ok = file:write_file(ImageFile, ImageBin),
		io:format("LBS: image saved to ~s~n", [ImageFile]);

	_Other ->
		io:format("LBS: image is not (yet?) available~n", [])
	end.

'ling-build-image'(Config, _AppFile) ->

	Continue = fun(ProjName, LingOpts) ->
		case get_build_status(ProjName, LingOpts) of
		ok ->
			retrieve_image(ProjName, LingOpts);
		failed ->
			io:format("LBS: **** build failed ****~n", [])
		end
	end,

	%% same as ling-build but with different continuation
	do_ling_build(Config, Continue).

get_build_status(ProjName, LingOpts) ->
	case build_service:call(get, "/build/" ++ ProjName ++ "/status",
										[], none, LingOpts) of
	{ok,"0"} ->
		ok;

	{ok,"1"} ->
		receive after 3000 -> ok end,
		get_build_status(ProjName, LingOpts);

	{ok,"99"} ->
		failed
	end.

add_misc_files([]) ->
	ok;
add_misc_files([{import,Pat}|LingOpts]) when is_list(Pat) ->

	Files = [filename:absname(F) || F <- filelib:wildcard(Pat)],
	ling_queue:add(Files),
	add_misc_files(LingOpts);
add_misc_files([_|LingOpts]) ->
	add_misc_files(LingOpts).

%%EOF
