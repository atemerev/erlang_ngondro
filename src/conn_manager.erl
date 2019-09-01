-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  io:format("INIT~n"),
  {ok, P};

proc({gun_ws, _, _, Msg}, #pi{state = {Conn, Timer, _}} = P) ->
  process_bitmex_message(Msg),
  {reply, [], P#pi{state = {Conn, Timer, stamp()}}};

proc({gun_down, _, _, Reason, _, _}, _) ->
  io:format("TIC DOWN: ~p~n", [Reason]),
  {reply, [], #pi{state = {[], [], []}}};

proc({gun_up, Conn, _}, P) ->
  io:format("TIC UP: ~p~n", [Conn]),
  gun:ws_upgrade(Conn, <<"/realtime">>, []),
  {reply, [], P};

proc({gun_upgrade, Conn, _, _, _}, #pi{state = {[], [], []}} = P) ->
  io:format("TIC WS: ~p~n", [Conn]),
  subscribe(Conn),
  {reply, [], P#pi{state = {Conn, timer_restart(), stamp()}}};

proc({gun_upgrade, Conn, _, _, _}, #pi{state = {_, Timer, _}} = P) ->
  erlang:cancel_timer(Timer),
  io:format("TIC WS: ~p~n", [Conn]),
  subscribe(Conn),
  {reply, [], P#pi{state = {Conn, timer_restart(), stamp()}}};

proc({gun_error, _, _, Reason}, P) ->
  io:format("TIC ERR: ~p~n", [Reason]),
  {reply, [], P};

proc({timer, ping}, #pi{state = {[], [], []} = P}) ->
  {reply, [], P};

proc({timer, ping}, #pi{state = {Conn, Timer, PrevTime}} = P) ->
  erlang:cancel_timer(Timer),
  D = stamp() - PrevTime,
  case D > 10000 of
    true -> spawn(fun() -> n2o_pi:restart(caching, "bitmex") end);
    false -> io:format("~p~n", [D]) end,
  {reply, [], P#pi{state = {Conn, timer_restart(), PrevTime}}};

proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

stamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

timer_restart() ->
  erlang:send_after(1000, self(), {timer, ping}).

process_bitmex_message(Msg) ->
  {text, Content} = Msg,
  Data = jsone:decode(Content),
  MaybeTable = maps:find(<<"table">>, Data),
  case MaybeTable of
    {ok, <<"orderBookL2">>} -> io:format("~s", ["."]);
    {ok, <<"trade">>} -> io:format("~s", ["|"]);
    _ -> do_nothing
  end.

subscribe(Conn) ->
  gun:ws_send(Conn, {text, "{\"op\": \"subscribe\", \"args\": [\"orderBookL2:XBTUSD\", \"trade:XBTUSD\"]}"}).