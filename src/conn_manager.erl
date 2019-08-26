-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

proc(init, P) ->
  {ok, _}      = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  InitialState = {[], timer_restart(), get_timestamp()},
  {ok, P#pi{state = InitialState}};

proc({gun_down, _, _, Reason, _, _}, P) ->
  io:format("TIC DOWN: ~p~n", [Reason]),
  {reply, [], P};

proc({gun_up, Conn, _}, #pi{state = {_, Timer, _}} = P) ->
  io:format("TIC UP: ~p~n", [Conn]),
  gun:ws_upgrade(Conn, "/realtime"),
  erlang:cancel_timer(Timer),
  {reply, [], P#pi{state={Conn, timer_restart(), get_timestamp()}}};

proc({gun_upgrade, Conn, _, _, _}, P) ->
  io:format("TIC WS: ~p~n", [Conn]),
  gun:ws_send(Conn, {text, "{\"op\": \"subscribe\", \"args\": [\"orderBookL2:XBTUSD\"]}"}),
  {reply, [], P};

proc({gun_error, _, _, Reason}, P) ->
  io:format("TIC ERR: ~p~n", [Reason]),
  {reply, [], P};

proc({gun_ws, _, _, Msg}, #pi{state = {Conn, Timer, _}} = P) ->
%%  io:format("TIC msg: ~p, ~p~n", [Msg, CurrentTime]),
  {reply, [], P#pi{state = {Conn, Timer, get_timestamp()}}};

proc({timer, ping}, #pi{state = {Conn, Timer, PrevTime}} = P) ->
  CurrentTime = get_timestamp(),
  if
    CurrentTime - PrevTime > 10000 ->
      io:format("TIC TIMEOUT: ~p~n", [CurrentTime - PrevTime]),
      case Conn of
        [] ->
          do_nothing;
        X when erlang:is_pid(X) ->
          gun:shutdown(Conn),
          #pi{name = Name, table = Tab} = P,
          n2o_pi:stop(Tab, Name)
      end,
      {reply, [], P#pi{state = {[], Timer, PrevTime}}};
    true ->
      io:format("TIC TS: prev: ~p, current: ~p, diff: ~p~n", [PrevTime, CurrentTime, CurrentTime - PrevTime]),
      {reply, [], P#pi{state = {Conn, timer_restart(), PrevTime}}}
  end;

proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

% Utilities

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

timer_restart() ->
  erlang:send_after(1000, self(), {timer, ping}).