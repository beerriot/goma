-module(goma_dispatcher).

-export([
         dispatch/2
        ]).

dispatch(Selector, Rules) ->
    find_match(re:split(Selector, "/"), Rules).

find_match(_, []) ->
    nomatch;
find_match(SelectorParts, [{Match, Module, Args}|Rest]) ->
    case is_match(SelectorParts, Match, []) of
        {true, Bindings} ->
            {ok, Bindings, Module, Args};
        false ->
            find_match(SelectorParts, Rest)
    end.

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
