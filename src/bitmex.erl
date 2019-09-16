-module(bitmex).
-include_lib("n2o/include/n2o.hrl").
-include("../include/state.hrl").
-compile(export_all).

proc(init, #pi{state = S} = P) ->
  {ok, _} = gun:open("www.bitmex.com", 443, #{protocols => [http], transport => tls}),
  io:format("INIT~n"),
  {ok, P#pi{state = S#venue_state{timer = timer_restart(), stamp = misc:timestamp()}}};

proc({gun_ws, _, _, Msg}, #pi{state = S} = P) ->
  NewState = process_bitmex_message(Msg, S),
  CurrentTime = misc:timestamp(),
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
  {reply, [], P#pi{state = S#venue_state{conn = Conn, timer = timer_restart(), stamp = misc:timestamp()}}};

proc({gun_upgrade, Conn, _, _, _}, #pi{state = #venue_state{timer = Timer} = S} = P) ->
  io:format("TIC WS2: ~p ~p~n", [Conn, Timer]),
  erlang:cancel_timer(Timer),
  subscribe(Conn),
  {reply, [], P#pi{state = S#venue_state{timer = timer_restart(), stamp = misc:timestamp()}}};

proc({gun_error, _, _, Reason}, P) ->
  io:format("TIC ERR: ~p~n", [Reason]),
  {reply, [], P};

proc({timer, ping}, #pi{state = #venue_state{conn = [], timer = []}} = P) ->
  {reply, [], P};

proc({timer, ping}, #pi{state = #venue_state{timer = Timer, stamp = PrevTime} = S} = P) ->
  erlang:cancel_timer(Timer),
  D = misc:timestamp() - PrevTime,
  case D > 10000 of
    true -> spawn(fun() -> n2o_pi:restart(caching, "bitmex") end);
    false -> io:format("(~p)", [D]) end,
  {reply, [], P#pi{state = S#venue_state{timer = timer_restart()}}};

proc(Unknown, P) ->
  io:format("TIC UNK: ~p~n", [Unknown]),
  {reply, [], P}.

cancel_all(#auth{} = Auth) ->
  %% todo assemble headers
  Expires = integer_to_list(misc:expires_time()),
  Headers = req_headers(Auth, "DELETE", "/api/v1/order/all", "", Expires),
  httpc:request(delete, {"https://www.bitmex.com/api/v1/order/all",
    Headers,
    "application/json", ""},
    [{ssl, [{versions, ['tlsv1.2']}]}], []).

req_headers(#auth{api_key = ApiKey, secret = Secret}, Verb, Path, Data, Expires) ->
  Signature = signature(Secret, Verb, Path, Data, Expires),
  [{"Accept", "application/json"},
    {"api-expires", Expires},
    {"api-key", ApiKey},
    {"api-signature", Signature}].

signature(Secret, Verb, Path, Data, Expires) ->
  Plain = io_lib:format("~s~s~s~s", [Verb, Path, Expires, Data]),
  Hmac = crypto:hmac(sha256, Secret, Plain),
  misc:bin_to_hex(Hmac).

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
      SpreadThreshold = application:get_env(syob, notify_spread),
      if
        Spread >= SpreadThreshold ->
          Auth = PrevState#venue_state.auth,
          cancel_all(Auth),
          n2o_pi:send(caching, "notifier", {notify, Spread});
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