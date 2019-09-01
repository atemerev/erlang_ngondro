-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-include("state.hrl").

-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  io:format("INIT~n"),
  {ok, P};

proc({gun_ws, _, _, Msg}, #pi{state = #venue_state{} = S} = P) ->
  process_bitmex_message(Msg),
  CurrentTime = stamp(),
  {reply, [], P#pi{state = S#venue_state{stamp = CurrentTime}}};

proc({gun_down, _, _, Reason, _, _}, _) ->
  io:format("TIC DOWN: ~p~n", [Reason]),
  EmptyBook = orderbook:new_book("XBTUSD"),
  {reply, [], #pi{state = #venue_state{conn = [], timer = [], stamp = [], orderbook = EmptyBook}}};

proc({gun_up, Conn, _}, P) ->
  io:format("TIC UP: ~p~n", [Conn]),
  gun:ws_upgrade(Conn, <<"/realtime">>, []),
  {reply, [], P};

proc({gun_upgrade, Conn, _, _, _}, #pi{state = #venue_state{conn = [], timer = []} = S} = P) ->
  io:format("TIC WS1: ~p~n", [Conn]),
  subscribe(Conn),
  CurrentTime = stamp(),
  {reply, [], P#pi{state = S#venue_state{conn = Conn, timer = timer_restart(), stamp = CurrentTime}}};

proc({gun_upgrade, Conn, _, _, _}, #pi{state = #venue_state{timer = Timer} = S} = P) ->
  io:format("TIC WS2: ~p ~p~n", [Conn, Timer]),
  erlang:cancel_timer(Timer),
  subscribe(Conn),
  CurrentTime = stamp(),
  {reply, [], P#pi{state = S#venue_state{timer = timer_restart(), stamp = CurrentTime}}};

proc({gun_error, _, _, Reason}, P) ->
  io:format("TIC ERR: ~p~n", [Reason]),
  {reply, [], P};

proc({timer, ping}, #pi{state = #venue_state{conn = [], timer = []}} = P) ->
  {reply, [], P};

proc({timer, ping}, #pi{state = #venue_state{timer = Timer, stamp = PrevTime} = S} = P) ->
  erlang:cancel_timer(Timer),
  D = stamp() - PrevTime,
  case D > 10000 of
    true -> spawn(fun() -> n2o_pi:restart(caching, "bitmex") end);
    false -> io:format("~p~n", [D]) end,
  {reply, [], P#pi{state = S#venue_state{timer = timer_restart()}}};

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