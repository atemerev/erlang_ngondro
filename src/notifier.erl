-module(notifier).
-author("atemerev").
-include_lib("n2o/include/n2o.hrl").
-include("state.hrl").

-compile(export_all).

proc(init, P) ->
  gun:open("api02.ethercast.net", 80, #{protocols => [http]}),
  io:format("Notifier started."),
  {ok, P};

proc({gun_up, Conn, _}, P) ->
  io:format("Notifier connected: ~p~n", [Conn]),
  {reply, [], P#pi{state = Conn}};

proc({notify, _}, #pi{state = []} = P) ->
  io:format("Can't notify: no connection.~n"),
  {reply, [], P};

proc({notify, Spread}, #pi{state = Conn} = P) ->
  io:format("Notify: ~p~n", [Spread]),
  Body = jsone:encode(#{spread => Spread}),
  Plaintext = <<"user:FitoJabaX">>,
  Base64 = base64:encode(Plaintext),
  StreamRef = gun:post(Conn, "/stream", [
    {<<"Content-Type">>, "application/json"},
    {<<"Authorization">>, io:format("Basic ~p", [Base64])}
  ], Body),
  case gun:await(Conn, StreamRef) of
    {response, fin, _, _} ->
      no_data;
    {response, nofin, _, _} ->
      {ok, body} = gun:await_body(Conn, StreamRef),
      io:format("~s~n", [Body])
  end,
  {reply, [], P}.





