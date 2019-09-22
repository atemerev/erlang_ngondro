-module(erlang_ngondro).
-include("../include/state.hrl").
-include_lib("n2o/include/n2o.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2, stop/1, init/1]).

start(_, _) ->
  logger:add_handlers(syob),
  client("bitmex"),
  notifier(),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_) -> ok.
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
notifier() ->
  InitialState = #notifier_state{last_notify = 0},
  n2o_pi:start(#pi{module = notifier, table = caching, sup = n2o, state = InitialState, name = "notifier"}).
client(Name) ->
  InitialState = #venue_state{conn=[], timer = [], stamp = [], orderbook = orderbook:new_book("XBTUSD")},
  n2o_pi:start(#pi{module = bitmex, table = caching, sup = n2o, state = InitialState, name = Name}).

