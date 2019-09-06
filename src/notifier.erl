-module(notifier).
-author("atemerev").
-include_lib("n2o/include/n2o.hrl").
-include("state.hrl").

-compile(export_all).

proc(init, P) ->
  io:format("Notifier started.~n"),
  {ok, P};

proc({gun_up, Conn, _}, P) ->
  {reply, [], P#pi{state = Conn}};

proc({notify, Spread}, #pi{} = P) ->
  io:format("Notify: ~p~n", [Spread]),
  {ok, Conn} = gun:open("api02.ethercast.net", 443, #{protocols => [http], transport => tls}),
  gun:await_up(Conn),
  Body = jsone:encode(#{spread => Spread}),
  Plaintext = <<"user:FitoJabaX">>,
  Base64 = base64:encode_to_string(Plaintext),
  StreamRef = gun:post(Conn, "/spread", [
    {<<"Content-Type">>, "application/json"},
    {<<"Authorization">>, io_lib:format("Basic ~s", [Base64])}
  ], Body),
  case gun:await(Conn, StreamRef) of
    {response, fin, _, _} ->
      no_data;
    {response, nofin, _, _} ->
      {ok, Response} = gun:await_body(Conn, StreamRef),
      io:format("Response: ~s~n", [Response])
  end,
  {reply, [], P};

proc(M,S) -> {reply, [], S}.