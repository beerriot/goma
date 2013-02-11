-module(goma_decision_core).

-export([start_link/4, init/4]).

-include_lib("kernel/include/file.hrl").
-include("goma.hrl").

-define(MAX_SELECTOR_LENGTH, 255).
-define(SELECTOR_TIMEOUT, 5000).

-record(ctx, {
          socket,
          transport,
          dispatch,
          path,
          module,
          modctx
         }).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport, Opts) ->
    ok = ranch:accept_ack(ListenerPid),
    Dispatch = proplists:get_value(dispatch, Opts),
    Ctx = #ctx{socket=Socket, transport=Transport, dispatch=Dispatch},
    try
        decision(v1a1, Ctx)
    catch Type:Reason ->
            error_logger:error_msg("~p:~p~n~p", 
                                   [Type, Reason, erlang:get_stacktrace()]),
            send_error(io_lib:format("Error -- ~p:~p", [Type, Reason]), Ctx)
    end,
    close_connection(Socket, Transport).

%% read request
decision(v1a1, Ctx) ->
    case goma_util:interpret_request(read_line(Ctx)) of
        {ok, Selector, Search} ->
            Path = #goma_selector{raw=Selector, search=Search},
            decision(v1a2, Ctx#ctx{path=Path});
        {error, Reason} ->
            send_error(Reason, Ctx)
    end;

%% dispatch
decision(v1a2, #ctx{path=P, dispatch=D}=Ctx) ->
    case goma_dispatcher:dispatch(P#goma_selector.raw, D) of
        {ok, Bindings, Module, Args} ->
            {ok, ModCtx} = Module:init(Args),
            case rcall(resource_exists,
                       Ctx#ctx{path=P#goma_selector{bindings=Bindings},
                               module=Module,
                               modctx=ModCtx}) of
                {true, NewCtx} ->
                    decision(v1a3, NewCtx);
                {false, NewCtx} ->
                    send_error("Resource not found", NewCtx)
            end;
        _ ->
            send_error("Resource not found", Ctx)
    end;

%% generate content
decision(v1a3, Ctx) ->
    {Content, NewCtx} = rcall(produce_content, Ctx),
    send_content(Content, NewCtx).

rcall(FunName, #ctx{path=Path, module=Mod, modctx=ModCtx}=Ctx) ->
    {Result, NewModCtx} = Mod:FunName(Path, ModCtx),
    {Result, Ctx#ctx{modctx=NewModCtx}}.

read_line(#ctx{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket,
                      [{recbuf, ?MAX_SELECTOR_LENGTH+2}, %% 2 == CRLF
                       {packet, line}]),
    Transport:recv(Socket, 0, ?SELECTOR_TIMEOUT).

send_error(Message, Ctx) ->
    FmtMsg = goma_util:format_menu([#goma_menu{type=?GOMA_TYPE_ERROR,
                                               display=Message}]),
    send_content({text, FmtMsg}, Ctx).

send_content({Type, Content}, #ctx{socket=Socket, transport=Transport}) ->
    Transport:send(Socket, Content),
    case Type of
        text ->
            Transport:send(Socket, <<"\r\n.\r\n">>);
        binary ->
            ok
    end.

close_connection(Socket, Transport) ->
    ok = Transport:close(Socket).
