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
      E1 = case MaybeNewPrice of
             Price when Price > 0 -> Entry#order_entry{price = Price};
             _ -> Entry
           end,
      E2 = case MaybeNewAmount of
             Amount when Amount > 0 -> Entry#order_entry{amount = Amount};
             _ -> E1
           end,
      NewLine = fputils:replace_at(Line, Idx, E2),
      put_line(Book, Side, NewLine)
  end.

get_line(_ = #orderbook{bids = Bids, offers = Offers}, Side) ->
  case Side of
    bid -> Bids;
    offer -> Offers;
    _ -> invalid_entry_side
  end.

put_line(Book = #orderbook{}, Side, NewLine) ->
  case Side of
    bid -> Book#orderbook{bids = NewLine};
    offer -> Book#orderbook{offers = NewLine};
    _ -> invalid_entry_side
  end.

best(_ = #orderbook{bids = Bids, offers = Offers}) ->
  BestBid = case Bids of
              [] -> not_found;
              [BH | _] -> BH#order_entry.price
            end,
  BestOffer = case Offers of
                [] -> not_found;
                [OH | _] -> OH#order_entry.price
              end,
  {BestBid, BestOffer}.