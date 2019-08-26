-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  {ok, P};
proc({gun_ws, _, _, Msg}, P) ->
  {_,_,Micro} = os:timestamp(),
  io:format("TIC msg: ~p~p~n", [Msg, Micro]),
  {reply, [], P#pi{state=Micro}};
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
proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

