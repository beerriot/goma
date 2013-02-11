
-module(goma_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Name, IP, Port, Dispatch) ->
    supervisor:start_link(?MODULE, [Name, IP, Port, Dispatch]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Name, IP, Port, Dispatch]) ->
    case whereis(ranch_sup) of
        undefined ->
            %% no one else has started ranch; put it in our tree
            RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
                            permanent, 5000, supervisor, [ranch_sup]};
        _ ->
            %% ranch is already started; watch it so we can restart if it does
            RanchSupSpec = {goma_ranch_watcher,
                            {goma_ranch_watcher, start_link, []},
                            permanent, 5000, server, [goma_ranch_watcher]}
    end,

    ListenerSpec = ranch:child_spec(Name,
                                    100,
                                    ranch_tcp,
                                    [{ip, IP}, {port, Port}],
                                    goma_decision_core,
                                    [{dispatch, bin_dispatch(Dispatch)}]),
    {ok, { {rest_for_one, 5, 10}, [RanchSupSpec, ListenerSpec]} }.

%% @doc ensure all rules use only binaries and atoms in their match spec
bin_dispatch(Dispatch) ->
    [ bin_rule(R) || R <- Dispatch ].

bin_rule({Match, Module, Arg}) ->
    {[ bin_string(M) || M <- Match ], Module, Arg}.

bin_string(S) when is_binary(S) ->
    S;
bin_string(A) when is_atom(A) ->
    A;
bin_string(L) when is_list(L) ->
    list_to_binary(L).
