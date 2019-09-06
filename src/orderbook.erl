-module(orderbook).
-author("atemerev").
-include("state.hrl").
-export([new_book/1, insert/2, delete/3, update/5, best/1]).

new_book(Symbol) ->
  #orderbook{symbol = Symbol, bids = [], offers = []}.

insert(Book = #orderbook{}, Entry = #order_entry{side = Side, price = Price}) ->
  Line = get_line(Book, Side),
  Fun = case Side of
          bid -> fun(X) -> Price > X#order_entry.price end;
          offer -> fun(X) -> Price < X#order_entry.price end;
          _ -> invalid_entry_side
        end,
  MaybeIdx = fputils:find_index(Line, Fun),
  NewLine = case MaybeIdx of
              not_found -> lists:append(Line, [Entry]);
              Idx -> fputils:insert_at(Line, Idx, Entry)
            end,
  put_line(Book, Side, NewLine).

delete(Book = #orderbook{}, Side, Id) ->
  Line = get_line(Book, Side),
  MaybeIdx = fputils:find_index(Line, fun(X) -> X#order_entry.id == Id end),
  NewLine = case MaybeIdx of
              not_found -> Line;
              Idx -> fputils:delete_at(Line, Idx)
            end,
  put_line(Book, Side, NewLine).

update(Book = #orderbook{}, Side, Id, _, 0) -> delete(Book, Side, Id);

update(Book = #orderbook{}, Side, Id, MaybeNewPrice, MaybeNewAmount) ->
  Line = get_line(Book, Side),
  MaybeIdxById = fputils:find_index(Line, fun(X) -> X#order_entry.id == Id end),

  case MaybeIdxById of
    not_found -> not_found;
    Idx ->
      Entry = lists:nth(Idx, Line),
      E1 = maybe_new(MaybeNewPrice,Entry,price),
      E2 = maybe_new(MaybeNewAmount,Entry,amount),
      NewLine = fputils:replace_at(Line, Idx, E2),
      put_line(Book, Side, NewLine)
  end.

maybe_new(Value,Entry,price) when Value > 0 -> Entry#order_entry{price = Value};
maybe_new(Value,Entry,amount) when Value > 0 -> Entry#order_entry{amount = Value};
maybe_new(Value,Entry,_) -> Entry.

get_line(#orderbook{bids = B, offers = O}, bid)   -> B;
get_line(#orderbook{bids = B, offers = O}, offer) -> O;
get_line(#orderbook{bids = B, offers = O}, _)     -> invalid_entry_side.

put_line(Book, bid, NewLine)   -> Book#orderbook{bids = NewLine};
put_line(Book, offer, NewLine) -> Book#orderbook{offers = NewLine};
put_line(Book, _, NewLine)     -> invalid_entry_side.

best([]) -> not_found;
best([H|_]) -> H#order_entry.price;
best(#orderbook{bids = Bids, offers = Offers}) -> {best(Bids),best(Offers)}.
