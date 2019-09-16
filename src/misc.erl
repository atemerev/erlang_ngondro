-module(misc).
-author("atemerev").

-export([expires_time/0, stamp/0]).

expires_time() ->
  {Mega, Sec, _} = os:timestamp(),
  (Mega * 1000000 + Sec + 10) * 1000.

stamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

