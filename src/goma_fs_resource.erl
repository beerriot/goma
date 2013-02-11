-module(goma_fs_resource).

-export([
         init/1,
         resource_exists/2,
         produce_content/2
        ]).

-include_lib("kernel/include/file.hrl").
-include("goma.hrl").

-record(ctx, {
          host,
          port,
          filebase
         }).

init(Args) ->
    {ok, #ctx{host = proplists:get_value(host, Args, "localhost"),
              port = proplists:get_value(port, Args, 70),
              filebase = proplists:get_value(base, Args, ".")}}.

resource_exists(Sel, Ctx) ->
    Info = file_info(Sel, Ctx),
    {is_record(Info, file_info), Ctx}.

produce_content(Sel, Ctx) ->
    Info = file_info(Sel, Ctx),
    case Info#file_info.type of
        directory ->
            Content = directory_menu(Sel, Ctx);
        regular ->
            Content = file_content(Sel, Ctx);
        Other ->
            Content = error_message(
                        io_lib:format("Unknown file type '~p'", [Other]))
    end,
    {Content, Ctx}.

directory_menu(Sel, Ctx) ->
    {ok, Names} = file:list_dir(file_path(Sel, Ctx)),
    Menu = lists:flatten([ menu_item(Sel, Ctx, Name) || Name <- Names ]),
    Content = goma_util:format_menu(Menu),
    {text, Content}.

file_content(Sel, Ctx) ->
    Path = file_path(Sel, Ctx),
    {ok, Data} = file:read_file(Path),
    {guess_text(Path), Data}.

error_message(Msg) ->
    {text, goma_util:format_menu([#goma_menu{type=?GOMA_TYPE_ERROR,
                                             display=Msg}])}.

-define(TEXT_FILE_EXT_RE,
        "\.(txt|md|org|html|css|erl|hrl|c|h|java)$").
guess_text(Path) ->
    case re:run(Path, ?TEXT_FILE_EXT_RE) of
        {match, _} ->
            text;
        nomatch ->
            binary
    end.

file_info(Sel, Ctx) ->
    case file:read_file_info(file_path(Sel, Ctx)) of
        {ok, Info} ->
            Info;
        {error, Reason} ->
            Reason
    end.

file_path(#goma_selector{bindings=B}, #ctx{filebase=Base}) ->
    Path = proplists:get_value('*', B),
    filename:join(Base, sanitize_path(Path)).

menu_item(#goma_selector{raw=Raw}=Sel,
          #ctx{host=Host, port=Port}=Ctx,
          Name) ->
    Fullname = filename:join(file_path(Sel, Ctx), Name),
    case file:read_file_info(Fullname) of
        {ok, #file_info{type=Type}} ->
            #goma_menu{
          type = menu_entry_type(Type, Name),
          display = Name,
          selector = goma_util:join_selector([sanitize_path(Raw), Name]),
          host = Host,
          port = Port}; % todo
        {error, Reason} ->
            #goma_menu{
          display = io_lib:format("Error '~p' for '~s'~n",
                                  [Reason, Fullname])}
    end.

menu_entry_type(directory, _) ->
    ?GOMA_TYPE_MENU;
menu_entry_type(regular, Name) ->
    case guess_text(Name) of
        text ->
            ?GOMA_TYPE_TEXT;
        binary ->
            ?GOMA_TYPE_BINARY
    end;
menu_entry_type(_, _) ->
    ?GOMA_TYPE_ERROR.

sanitize_path(Path) ->
    list_to_binary(re:replace(re:replace(Path, "\\.\\.", "", [global]),
                              "^/*", "")).
