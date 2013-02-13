GOpher MAchine ("goma") is a toolkit for serving resources over
[The Internet Gopher Protocol](http://tools.ietf.org/html/rfc1436). Its
design is influenced by
[Webmachine](https://github.com/basho/webmachine), in that
applications provide resource modules, the functions of which are
called in order to navigate the decisions of a
[flowchart](https://github.com/beerriot/goma/issues/1).

## A simple resource

This is your basic Hello World resource:

```erlang
-module(goma_hw).

-compile([export_all]).

init(Args) ->
    {ok, Args}.

resource_exists(_Selector, Context) ->
    {true, Context}.

produce_content(Selector, Context) ->
    Content = io_lib:format("Hello ~s World!",
                            [Selector#goma_selector.raw]),
    {{text, Content}, Context}.
```

[The wiki](https://github.com/beerriot/goma/wiki) has more information
about what the parameters and expected outputs of these functions. A
more interesting resource is found in
[goma_fs_resource.erl](https://github.com/beerriot/goma/blob/master/src/goma_fs_resource.erl).

## Dispatching

Goma chooses the module to handle a selector by splitting the selector
on `/` characters, and then comparing the pieces to a dispatch
table. The dispatch table is a list of 3-tuples of the form:

```erlang
{SelectorMatch, ModuleName, InitArg}
```

The first element, `SelectorMatch` is a list of strings and
atoms. Each string must match its corresponding selector piece
exactly. Each atom is "bound" to its corresponding selector piece, and
a list of those bindings is passed to the module's functions. The atom
`'*'` is special: it must come last, and it matches the rest of the
selector.

As an example, the `SelectorMatch`

```erlang
["foo", bar, '*']
```

Would match the selector

```
foo/bar/baz/quux
```

And create bindings of

```erlang
[{bar, <<"bar">>}, {'*', <<"baz/quux">>}]
```

## Startup

To startup goma with your dispatch table, use `goma:child_spec/1` to
create a supervisor child specification, and add that child to your
app's supervisor. For example:

```erlang
-module(myapp_sup).

%% ...other supervisor functions...

init([]) ->
    Name = myapp_goma,
    IP = {127,0,0,1},
    Port = 7070,
    Dispatch = {['*'], goma_hw, []},
    GomaSupSpec = goma:child_spec(Name, IP, Port, Dispatch),
    {ok, {{one_for_one,5,10}, [GomaSupSpec]}}.
```
