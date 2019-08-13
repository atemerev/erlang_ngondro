-module(conn_manager).
-compile(export_all).

proc(init, P) ->
  {ok, Conn} = gun:open("www.bitmex.com", 443),
  gun:ws_upgrade(Conn, "/realtime"),
  {ok, P};
proc({gun_ws, _, _, Msg}, P) ->
  io:format("~s~n", Msg),
  {reply, [], P};
proc({gun_down, _, _, Reason, _, _}, P) ->
  io:format("Gun down: ~s~n", Reason),
  {reply, [], P}.