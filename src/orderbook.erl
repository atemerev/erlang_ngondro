-module(orderbook).
-author("atemerev").
-include("state.hrl").
-export([new_book/1, insert/2, delete/3, update/5]).

new_book(Symbol) ->
  #orderbook{symbol = Symbol, bids = [], offers = []}.

insert(Book = #orderbook{bids = Bids, offers = Offers}, Entry = #order_entry{side = Side, price = Price}) ->
  {Line, Fun} = case Side of
                  bid -> {Bids, fun(X) -> Price < X end};
                  offer -> {Offers, fun(X) -> Price < X end};
                  _ -> {unknown, invalid_entry_side}
                end,

  MaybeIdx = fputils:find_index(Line, Fun),
  NewLine = case MaybeIdx of
              not_found -> lists:append(Line, [Entry]);
              Idx -> fputils:insert_at(Line, Idx, Entry)
            end,

  case Side of
    bid -> Book#orderbook{bids = NewLine};
    offer -> Book#orderbook{offers = NewLine};
    _ -> invalid_entry_side
  end.

delete(Book = #orderbook{bids = Bids, offers = Offers}, Side, Id) ->
  Line = case Side of
           bid -> Bids;
           offer -> Offers;
           _ -> invalid_entry_side
         end,
  MaybeIdx = fputils:find_index(Line, fun(X) -> Id == X end),
  NewLine = case MaybeIdx of
              not_found -> Line;
              Idx -> fputils:delete_at(Line, Idx)
            end,
  case Side of
    bid -> Book#orderbook{bids = NewLine};
    offer -> Book#orderbook{offers = NewLine};
    _ -> invalid_entry_side
  end.

update(Book = #orderbook{}, Side, Id, _, 0) -> delete(Book, Side, Id);

update(Book = #orderbook{bids = Bids, offers = Offers}, Side, Id, MaybeNewPrice, MaybeNewAmount) ->
  Line = case Side of
           bid -> Bids;
           offer -> Offers;
           _ -> invalid_entry_side
         end,
  MaybeIdxById = fputils:find_index(Line, fun(X) -> X == Id end),

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
      case Side of
        bid -> Book#orderbook{bids = NewLine};
        offer -> Book#orderbook{offers = NewLine};
        _ -> invalid_entry_side
      end
  end.

