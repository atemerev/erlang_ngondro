-module(fputils).
-author("atemerev").

-export([find_index/2, insert_at/3, delete_at/2, replace_at/3]).

find_index(List, Fun) -> find_index(List, Fun, 1).

find_index([], _, _) -> not_found;

find_index([H | T],  Fun, Idx) ->
  Res = Fun(H),
  case Res of
    true -> Idx;
    _ -> find_index(T, Fun, Idx + 1)
  end.

insert_at(List, Idx, Value) ->
  {Left, Right} = lists:split(Idx - 1, List),
  Left ++ [Value | Right].

delete_at(List, Idx) ->
  {Left, Right} = lists:split(Idx - 1, List),
  [_ | T] = Right,
  Left ++ T.

replace_at(List, Idx, Value) ->
  {Left, Right} = lists:split(Idx - 1, List),
  [_ | T] = Right,
  Left ++ [Value | T].