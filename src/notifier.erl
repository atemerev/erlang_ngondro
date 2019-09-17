-module(notifier).
-author("atemerev").
-include_lib("n2o/include/n2o.hrl").
-include("../include/state.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, P};

proc({notify, Spread}, #pi{state = #notifier_state{last_notify = Last}} = P) ->
  Current = misc:timestamp(),
  {ok, SpreadThreshold} = application:get_env(syob, notify_spread),
  {ok, Debounce} = application:get_env(syob, notify_debounce),
  {ok, ApiKey} = application:get_env(syob, bitmex_api_key),
  {ok, Secret} = application:get_env(syob, bitmex_secret),
  Auth = #auth{api_key = ApiKey, secret = Secret},

  if
    (Current - Last) > Debounce ->

      Content = #{ping => 1, current_spread => Spread},
      Response = notify_request(Content),
      io:format("[~p]", [Spread]),

      if
        Spread >= SpreadThreshold ->
          io:format("Spread detected: ~p, threshold: ~p~n", [Spread, SpreadThreshold]),
          Content = #{spread => Spread},
          Response = notify_request(Content),
          io:format("Notify: ~p~n", [jsone:decode(list_to_binary(Response))]),
          bitmex:cancel_all(Auth);
        true ->
          do_nothing
      end,

      NextState = #notifier_state{last_notify = Current},
      {reply, [], P#pi{state = NextState}};
    true ->
      {reply, [], P}
  end; % otherwise do nothing


proc(_, P) -> {reply, [], P}.

notify_request(Content) ->
  {ok, Endpoint} = application:get_env(syob, notify_endpoint),
  {ok, Creds} = application:get_env(syob, notify_credentials),
  {ok, {{"HTTP/1.1", 200, "OK"}, _, Response}} = httpc:request(post, {Endpoint,
    [{"Authorization", io_lib:format("Basic ~s",
      [base64:encode_to_string(Creds)])}],
    "application/json", jsone:encode(Content)},
    [{ssl, [{versions, ['tlsv1.2']}]}], []),
  Response.
