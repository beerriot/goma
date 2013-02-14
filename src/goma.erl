%% @doc GOpher MAchine: an Gopher protocol toolkit for Erlang.
-module(goma).

-export([
         child_spec/4
        ]).

%% @doc Create a child spec for starting a gopher machine interface.
-spec child_spec(atom(), inet:ip_address(), non_neg_integer(),
                 [goma_dispatch:rule()]) -> supervisor:child_spec().
child_spec(Name, IP, Port, Dispatch) ->
    {goma_sup, {goma_sup, start_link, [Name, IP, Port, Dispatch]},
     permanent, 5000, supervisor, [goma_sup]}.
