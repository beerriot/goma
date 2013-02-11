-module(goma).

-export([
         child_spec/4
        ]).

child_spec(Name, IP, Port, Dispatch) ->
    {goma_sup, {goma_sup, start_link, [Name, IP, Port, Dispatch]},
     permanent, 5000, supervisor, [goma_sup]}.
