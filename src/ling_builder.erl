-module(ling_builder).

-export(['ling-build'/2,'ling-image'/2,'ling-build-image'/2]).
-export(['ling-build-ec2'/2,'ling-ec2-image'/2,'ling-build-ec2-image'/2]).

'ling-build'(Config, _AppFile) ->
	do_ling_build(Config, elf, fun(_ProjName, _LingOpts) -> ok end).

'ling-build-ec2'(Config, _AppFile) ->
	do_ling_build(Config, ami, fun(_ProjName, _LingOpts) -> ok end).

do_ling_build(Config, ImageType, Continue) ->

	case whereis(ling_queue) of
	undefined ->
		ling_queue:start_link();
	_ ->
		ok
	end,

	BaseDir = my_base_dir(Config),
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
		LingOpts0 = rebar_config:get(Config, ling_builder_opts, []),
		LingOpts = [{image_type,ImageType}|LingOpts0],
		add_misc_files(LingOpts),
		ling_queue:start_build(ProjName, LingOpts),

		Continue(ProjName, LingOpts);

	true ->
		skip
	end,

	ok.

'ling-image'(Config, _AppFile) ->

	BaseDir = my_base_dir(Config),
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

'ling-ec2-image'(Config, _AppFile) ->

	BaseDir = my_base_dir(Config),
	IsBaseDir = rebar_utils:get_cwd() =:= BaseDir,

	if not IsBaseDir ->
		ok;
	true ->

		rebar_log:log(info, "starting inets~n", []),
		application:start(crypto),
		application:start(asn1),
		application:start(public_key),
		application:start(ssl),
		application:start(inets),

		ProjName = filename:basename(BaseDir),
		LingOpts = rebar_config:get(Config, ling_builder_opts, []),

		retrieve_ami_id(ProjName, LingOpts)
	end.

retrieve_ami_id(ProjName, LingOpts) ->
	Hdrs = [{"Accept","text/plain"}],
	case build_service:call(get, "/build/" ++ ProjName ++ "/ami_id",
									Hdrs, none, LingOpts) of
	{ok,AmiId} ->
		io:format("LBS: AMI generated successfuly: ~s~n", [AmiId]);

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
	do_ling_build(Config, elf, Continue).

'ling-build-ec2-image'(Config, _AppFile) ->

	Continue = fun(ProjName, LingOpts) ->
		case get_build_status(ProjName, LingOpts) of
		ok ->
			retrieve_ami_id(ProjName, LingOpts);
		failed ->
			io:format("LBS: **** build failed ****~n", [])
		end
	end,

	%% same as ling-build but with different continuation
	do_ling_build(Config, ami, Continue).

get_build_status(ProjName, LingOpts) ->
	case build_service:call(get, "/build/" ++ ProjName ++ "/status",
										[], none, LingOpts) of
	{ok,"0"} ->
		receive after 3000 -> ok end,
		get_build_status(ProjName, LingOpts);

	{ok,"1"} ->
		ok;

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

%%
%% rebar API with respect to determining the base directory changed at least two
%% times. This function hides differences between rebar versions.
%%
my_base_dir(Config) ->
	case erlang:function_exported(rebar_config, get_xconf, 2) of
	true ->
		rebar_config:get_xconf(Config, base_dir);
	false ->
		rebar_config:get_global(base_dir, undefined)
	end.

%%EOF
