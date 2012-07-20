-module(ling_builder).

-export(['ling-build'/2]).

'ling-build'(Config, AppFile) ->
	io:format("ling-build: Config ~p AppFile ~p~n", [Config, AppFile]).

%%EOF
