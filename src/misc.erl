-module(misc).
-author("atemerev").

-export([expires_time/0, timestamp/0, bin_to_hex/1]).

expires_time() ->
  {Mega, Sec, _} = os:timestamp(),
  (Mega * 1000000 + Sec + 10) * 1000.

timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000 + round(Micro / 1000).

bin_to_hex(Bin) -> lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]).

byte_to_hex(<< N1:4, N2:4 >>) ->
  [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
