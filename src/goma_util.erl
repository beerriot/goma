%% @doc Some useful functions for dealing with the Gopher protocol.
-module(goma_util).

-export([
         interpret_request/1,
         join_selector/1,
         format_menu/1
        ]).

-include("goma.hrl").

%% @doc separate the selector from the search in the given string. The
%% selector is everything from the start up to the first tab or
%% newline. The search is everything from the first tab to the first
%% newline.
%%
%% This function accepts either `iodata()' or `{ok, iodata()}' and
%% `{error,term()}', so you can pass the result of most `read(...)'
%% calls directly to it.
-spec interpret_request({ok, iodata()}|iodata()|{error, term()}) ->
         {ok, Selector :: binary(), Search :: binary() | undefined}
        |{error, term()}.
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

%% @doc Join the given parts into a single selector by placing `/'
%% characters between them. Example:
%%    `join_selector(["a","b","c"]) -> <<"a/b/c">>'
-spec join_selector([iodata()]) -> binary().
join_selector(Parts) ->
    %% reversing instead of using foldr so we can start the
    %% accumulator with the last tail
    case lists:reverse(Parts) of
        [Head|Tail] ->
            list_to_binary(
              lists:foldl(fun(P, A) -> [P, "/"|A] end, [Head], Tail));
        [] ->
            <<>>
    end.

%% @doc Turn one or more `#goma_menu{}' records into a text menu for
%% sending to a client.
-spec format_menu(#goma_menu{}|[#goma_menu{}]) -> iolist().
format_menu(#goma_menu{type=T, display=D, selector=S, host=H, port=P}) ->
    io_lib:format("~s~s\t~s\t~s\t~b\r\n", [T, D, S, H, P]);
format_menu(Menu) when is_list(Menu) ->
    [ format_menu(I) || I <- Menu ].
