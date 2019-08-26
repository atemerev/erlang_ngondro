-module(erlang_ngondro).
-include_lib("n2o/include/n2o.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2, stop/1, init/1]).

start(_, _) -> client("bitmex"), supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_) -> ok.
init([]) -> {ok, { {one_for_one, 5, 10}, []} }.
client(Name) -> n2o_pi:start(#pi{module=conn_manager,table=caching,sup=n2o,state=[0],name=Name}).

