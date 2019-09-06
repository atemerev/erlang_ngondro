-module(erlang_ngondro).
-include("state.hrl").
-include_lib("n2o/include/n2o.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2, stop/1, init/1]).

start(_, _) ->
  client("bitmex"),
  notifier(),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_) -> ok.
init([]) -> {ok, {{one_for_one, 5, 10}, []}}.
notifier() ->
  n2o_pi:start(#pi{module = notifier, table = caching, sup = n2o, state = [], name = "notifier"}).
client(Name) ->
  InitialState = #venue_state{conn=[], timer = [], stamp = [], orderbook = orderbook:new_book("XBTUSD")},
  n2o_pi:start(#pi{module = conn_manager, table = caching, sup = n2o, state = InitialState, name = Name}).

