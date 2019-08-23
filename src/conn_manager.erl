-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  {ok, P};
proc({gun_ws, _, _, Msg}, P) ->
  io:format("TIC msg: ~p~n", [Msg]),
  {reply, [], P};
proc({gun_down, _, _, Reason, _, _}, P) ->
  io:format("TIC DOWN: ~p~n", [Reason]),
  {reply, [], P};
proc({gun_up, Conn, _}, P) ->
  gun:ws_upgrade(Conn, "/realtime"),
  {reply, [], P};
proc({gun_upgrade, Conn, _, _, _}, P) ->
  io:format("TIC WS: ~p~n", [Conn]),
  {reply, [], P};
proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

