-module(notifier).
-author("atemerev").
-include_lib("n2o/include/n2o.hrl").
-include("../include/state.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, P};

proc({notify, Spread}, #pi{state = #notifier_state{last_notify = Last} = S} = P) ->
  Current = misc:timestamp(),
  Debounce = application:get_env(syob, notify_debounce),
  if
    (Current - Last) > Debounce ->
      Endpoint = application:get_env(syob, notify_endpoint),
      Creds = application:get_env(syob, notify_credentials),
      BasicAuth = io_lib:format("Basic ~s", [base64:encode_to_string(Creds)]),
      Headers = [{"Authorization",    BasicAuth},
                  "application/json", jsone:encode(#{spread => Spread})],

      {ok, {{"HTTP/1.1", 200, "OK"}, _, Response}} =
        httpc:request(post, {Endpoint, Headers},
          [{ssl, [{versions, ['tlsv1.2']}]}], []),
      io:format("Notify: ~p~n", [jsone:decode(list_to_binary(Response))]),
      NextState = #notifier_state{last_notify = Current},
      {reply, [], P#pi{state = NextState}};
    true -> ok
  end;

proc(_, P) -> {reply, [], P}.
