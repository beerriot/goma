%% Resource types
-define(GOMA_TYPE_TEXT, <<"0">>).
-define(GOMA_TYPE_MENU, <<"1">>).
-define(GOMA_TYPE_ERROR, <<"3">>).
-define(GOMA_TYPE_SEARCH, <<"7">>).
-define(GOMA_TYPE_BINARY, <<"9">>).
-define(GOMA_TYPE_INFO, <<"i">>).

-record(goma_menu, {
          type = ?GOMA_TYPE_INFO :: iodata(),  % one of the GOMA_TYPE_* macros
          display = <<>> :: iodata(),          % display string
          selector = <<>> :: iodata(),         % selector string
          host = <<"error.host">> :: iodata(), % host string
          port = 70 :: non_neg_integer()       % port
         }).

-record(goma_selector, {
          bindings = [] :: [{atom(), binary()}], % dispatch bindings
          raw = <<>> :: binary(),        % unprocessed (but split from search)
          search :: binary() | undefined         % search
         }).
