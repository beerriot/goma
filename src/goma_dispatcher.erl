%% @doc Choosing a module to handle a request by matching the selector
%% to a set of rules. In an effort to prevent duplication, please see
%% the examples on the wiki:
%% [https://github.com/beerriot/goma/wiki/Dispatching]
-module(goma_dispatcher).

-export([
         dispatch/2
        ]).

-export_type([rule/0]).

-type rule() :: {Match :: match(), Module :: atom(), Argument :: term()}.
-type match() :: [string()|binary()|atom()].
-type binding() :: {Name :: atom(), Value :: binary()}.

%% @doc Find a rule matching the selector.
-spec dispatch(binary(), [rule()]) ->
         {ok, [binding()], Module :: atom(), Argument :: term()}
        |nomatch.
dispatch(Selector, Rules) ->
    Parts = case re:split(Selector, "/") of
                [<<>>|Rest] ->
                    %% this also translates the empty selector
                    %% (Selector == <<>>) to the empty list []
                    Rest;
                Other -> Other
            end,
    find_match(Parts, Rules).

find_match(_, []) ->
    nomatch;
find_match(SelectorParts, [{Match, Module, Args}|Rest]) ->
    case is_match(SelectorParts, Match, []) of
        {true, Bindings} ->
            {ok, Bindings, Module, Args};
        false ->
            find_match(SelectorParts, Rest)
    end.

-spec is_match([binary()], match(), [binding()]) ->
        {true, [binding()]} | false.
is_match([], [], Bindings) ->
    {true, Bindings};
is_match(Parts, ['*'], Bindings) ->
    {true, [{'*', goma_util:join_selector(Parts)}|Bindings]};
is_match([Next|RestParts], [Next|RestMatch], Bindings) ->
    is_match(RestParts, RestMatch, Bindings);
is_match([Part|RestParts], [Match|RestMatch], Bindings) when is_atom(Match) ->
    is_match(RestParts, RestMatch, [{Match, Part}|Bindings]);
is_match(_, _, _) ->
    false.
