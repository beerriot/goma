-module(goma_util).

-export([
         interpret_request/1,
         join_selector/1,
         format_menu/1
        ]).

-include("goma.hrl").

interpret_request({ok, Data}) ->
    interpret_request(Data);
interpret_request({error,_}=Error) ->
    Error;
interpret_request(Data) when is_binary(Data); is_list(Data) ->
    case re:run(Data, "([^\t\n\r]+)\t*([^\n\r]*)",
                [{capture, all_but_first, binary}]) of
        {match, [Selector, <<>>]} ->
            {ok, Selector, undefined};
        {match, [Selector, Search]} ->
            {ok, Selector, Search};
        nomatch ->
            {ok, <<>>, undefined}
    end.

join_selector(Parts) ->
    %% reversing instead of using foldr so we can start the
    %% accumulator with the last tail
    case lists:reverse(Parts) of
        [Head|Tail] ->
            list_to_binary(
              lists:foldl(fun(P, A) -> [P, "/"|A] end, [Head], Tail));
        [] ->
            []
    end.

format_menu(#goma_menu{type=T, display=D, selector=S, host=H, port=P}) ->
    io_lib:format("~s~s\t~s\t~s\t~b\r\n", [T, D, S, H, P]);
format_menu(Menu) when is_list(Menu) ->
    [ format_menu(I) || I <- Menu ].
