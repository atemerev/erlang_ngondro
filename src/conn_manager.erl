-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  CurrentTime = get_timestamp(),
  InitialState = {[], timer_restart(), CurrentTime},
  {ok, P#pi{state = InitialState}};
proc({gun_ws, _, _, Msg}, #pi{state = {Conn, Timer, _}} = P) ->
  CurrentTime = get_timestamp(),
%%  io:format("TIC msg: ~p, ~p~n", [Msg, CurrentTime]),
  {reply, [], P#pi{state = {Conn, Timer, CurrentTime}}};
proc({gun_down, _, _, Reason, _, _}, P) ->
  io:format("TIC DOWN: ~p~n", [Reason]),
  {reply, [], P};
proc({gun_up, Conn, _}, P) ->
  gun:ws_upgrade(Conn, "/realtime"),
  {reply, [], P};
proc({gun_upgrade, Conn, _, _, _}, P) ->
  io:format("TIC WS: ~p~n", [Conn]),
  gun:ws_send(Conn, {text, "{\"op\": \"subscribe\", \"args\": [\"orderBookL2:XBTUSD\"]}"}),
  {reply, [], P};
proc({timer, ping}, #pi{state = {Conn, _, PrevTime}} = P) ->
  CurrentTime = get_timestamp(),
  io:format("TIC prev: ~p, current: ~p~n", [PrevTime, CurrentTime]),
  {reply, [], P#pi{state = {Conn, timer_restart(), PrevTime}}};
proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

timer_restart() ->
  erlang:send_after(1000, self(), {timer, ping}).