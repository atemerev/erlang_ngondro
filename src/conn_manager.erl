-module(conn_manager).
-include_lib("n2o/include/n2o.hrl").
-include("state.hrl").

-compile(export_all).

proc(init, P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
%%  n2o_pi:start(#pi{module = notifier, table = caching, sup = n2o, state = [], name = "notifier"}),
  io:format("INIT~n"),
  {ok, P};

proc({gun_ws, _, _, Msg}, #pi{state = S} = P) ->
  NewState = process_bitmex_message(Msg, S),
  CurrentTime = stamp(),
  {reply, [], P#pi{state = NewState#venue_state{stamp = CurrentTime}}};

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
    false -> io:format("(~p)", [D]) end,
  {reply, [], P#pi{state = S#venue_state{timer = timer_restart()}}};

proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

stamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

timer_restart() ->
  erlang:send_after(1000, self(), {timer, ping}).

process_bitmex_message(Msg, PrevState) ->
  {text, Content} = Msg,
  Data = jsone:decode(Content),
  MaybeTable = maps:find(<<"table">>, Data),
  case MaybeTable of
    {ok, <<"orderBookL2">>} ->
      Action = maps:get(<<"action">>, Data),
      Entries = maps:get(<<"data">>, Data),
      NewBook = case Action of
                  <<"partial">> ->
                    EmptyBook = orderbook:new_book("XBTUSD"),
                    io:format("~s", ["="]),
                    insert_entries(EmptyBook, Entries);
                  <<"insert">> ->
                    io:format("~s", ["+"]),
                    insert_entries(PrevState#venue_state.orderbook, Entries);
                  <<"update">> ->
                    PrevBook = PrevState#venue_state.orderbook,
                    io:format("~s", ["."]),
                    lists:foldl(
                      fun(E, B) ->
                        #{<<"id">> := Id, <<"side">> := Side} = E,
                        MaybeNewPrice = maps:get(<<"price">>, E, none),
                        MaybeNewAmount = maps:get(<<"size">>, E, none),
                        EntrySide = parse_side(Side),
                        orderbook:update(B, EntrySide, Id, MaybeNewPrice, MaybeNewAmount)
                      end, PrevBook, Entries
                    );
                  <<"delete">> ->
                    PrevBook = PrevState#venue_state.orderbook,
                    io:format("~s", ["-"]),
                    lists:foldl(
                      fun(E, B) ->
                        #{<<"id">> := Id, <<"side">> := Side} = E,
                        EntrySide = parse_side(Side),
                        orderbook:delete(B, EntrySide, Id)
                      end, PrevBook, Entries
                    )
                end,
      {BestBid, BestOffer} = orderbook:best(NewBook),
      Spread = BestOffer - BestBid,
      if
        Spread > 5 -> n2o_pi:send(caching, "notifier", {notify, Spread});
        true -> do_nothing
      end,
      PrevState#venue_state{orderbook = NewBook};
    {ok, <<"trade">>} ->
      io:format("~s", ["|"]),
      PrevState;
    _ -> PrevState
  end.

parse_order_entry(Term) ->
  #{<<"id">> := Id, <<"side">> := Side, <<"size">> := Size, <<"price">> := Price} = Term,
  EntrySide = parse_side(Side),
  #order_entry{id = Id, side = EntrySide, price = Price, amount = Size}.

parse_side(RawSide) ->
  case RawSide of
    <<"Buy">> -> bid;
    <<"Sell">> -> offer
  end.

insert_entries(Book, Entries) ->
  lists:foldl(
    fun(E, B) ->
      % todo check for symbol
      Entry = parse_order_entry(E),
      orderbook:insert(B, Entry)
    end, Book, Entries).


subscribe(Conn) ->
  gun:ws_send(Conn, {text, "{\"op\": \"subscribe\", \"args\": [\"orderBookL2:XBTUSD\", \"trade:XBTUSD\"]}"}).